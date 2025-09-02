RipOverworldCharacters();
RipOverworldMap();
RipOverworldSprites();

void RipOverworldCharacters()
{
	var tilemaps = RipOverworldTilemaps();
	var paletteMaps = RipOverworldTilePaletteMaps();
	// AFAIK, no tiles are reused with different palettes.  So we'll just store "the" palette
	// for each tile.  We'll shift it up by 2 bits so we can OR it with the tile data to get
	// a 4bpp color.
	var tilePalettes = new byte[0x100];
	for (int i = 0; i < 0x80; i++)
	{
		tilePalettes[tilemaps[i]]         = (byte)((paletteMaps[i] << 2) & 0x0C);
		tilePalettes[tilemaps[i + 0x80]]  = (byte)(paletteMaps[i] & 0x0C);
		tilePalettes[tilemaps[i + 0x100]] = (byte)((paletteMaps[i] >> 2) & 0x0C);
		tilePalettes[tilemaps[i + 0x180]] = (byte)((paletteMaps[i] >> 4) & 0x0C);
	}

	using var readStream = File.Open("ff1.nes", FileMode.Open, FileAccess.Read);
	using var writeStream = File.Open("overworld-chr.m7", FileMode.Create, FileAccess.Write);

	var tileData2bpp = new byte[0x10]; // one 2bpp tile is 16 bytes
	var tileData8bpp = new byte[0x40]; // one 8bpp tile is 64 bytes
	readStream.Position = 0x8010; // Overworld character data starts at the beginning of bank 02
	for (int i = 0; i < 0x100; i++) // 256 tiles
	{
		var palette = tilePalettes[i];
		readStream.Read(tileData2bpp, 0, 0x10); // Read one 2bpp character (16 bytes)
		for (int y = 0; y < 8; y++) // 8 rows of pixels
		{
			for (int x = 0; x < 8; x++) // 8 pixels per row
			{
				var lowBit = tileData2bpp[y] >> (7 - x) & 1;
				var highBit = tileData2bpp[y + 8] >> (7 - x) & 1;
				tileData8bpp[y * 8 + x] = (byte)((highBit << 1) | lowBit | palette);
			}
		}
		writeStream.Write(tileData8bpp, 0, 0x40); // Write one 8bpp character (64 bytes)
	}

	readStream.Close();
	writeStream.Close();
}

byte[] RipOverworldTilemaps()
{
	using var readStream = File.Open("ff1.nes", FileMode.Open, FileAccess.Read);
	using var writeStream = File.Open("overworld-tilemaps.bin", FileMode.Create, FileAccess.Write);

	// There are 128 tiles.  The tilemap stores the upper-left character of each tile, then the upper-right, lower-left, and lower-right.
	// A character index is one byte, so the whole tilemap is 512 bytes.
	var tilemapData = new byte[0x200];
	readStream.Position = 0x0110; // Overworld tilemap data starts at 0x0100 in bank 00
	readStream.Read(tilemapData, 0, tilemapData.Length); // Read the entire tilemap
	writeStream.Write(tilemapData, 0, tilemapData.Length); // Write the tilemap to file

	readStream.Close();
	writeStream.Close();

	return tilemapData;
}

byte[] RipOverworldTilePaletteMaps()
{
	using var readStream = File.Open("ff1.nes", FileMode.Open, FileAccess.Read);

	// There are 128 tiles, each made of 4 characters, and each character has a palette.
	// A palette index is only 2 bits, so this data fits into 128 bytes.
	var paletteMapData = new byte[0x80];
	readStream.Position = 0x0310; // Overworld palette map data starts at 0x0300 in bank 00
	readStream.Read(paletteMapData, 0, paletteMapData.Length); // Read the entire palette map
	readStream.Close();

	// We don't need to write these, because we'll just apply each palette to the tile data
	// to make them 4bpp.

	return paletteMapData;
}

void RipOverworldMap()
{
	using var readStream = File.Open("ff1.nes", FileMode.Open, FileAccess.Read);
	using var writeStream = File.Open("overworld-map.bin", FileMode.Create, FileAccess.Write);

	// The overworld map is 256 RLE compressed rows, each having a 2-byte pointer.
	// The pointers are stored in the first 512 bytes, and they index into the map
	// data.  The compressed data doesn't QUITE fill up the entire bank, but it's
	// close, so we can just dump the whole thing here.
	// There is some unrelated data at the end of this bank, about 160 bytes.
	// Dunno what it is.
	var mapData = new byte[0x4000];
	readStream.Position = 0x4010; // Overworld map data starts at the beginning of bank 01
	readStream.Read(mapData, 0, mapData.Length); // Read the entire map, including the pointers
	for (int i = 1; i < 0x0200; i+=2) // we need to fix the pointers
	{
		mapData[i] &= 0x7F; // these pointers are relative to 0x8000, but we want them to be zero-based
	}
	writeStream.Write(mapData, 0, mapData.Length); // Write the map to file

	readStream.Close();
	writeStream.Close();
}

void RipOverworldSprites()
{
	using var readStream = File.Open("ff1.nes", FileMode.Open, FileAccess.Read);
	using var writeStream = File.Open("overworld-sprites.4bpp", FileMode.Create, FileAccess.Write);

	var spriteData2bpp = new byte[0x1180];
	readStream.Position = 0x9010; // start of sprite data
	readStream.Read(spriteData2bpp, 0, spriteData2bpp.Length); // Read all the sprite data

	// All of these sprites are 16x16, stored as 8x8 tiles, UL-UR-LL-LR.
	// We need to convert them to 4bpp SNES format, and put the lower two tiles 16 tiles after
	// the upper two tiles.

	// Start with the player sprites.  There are 12 of these, one for each class.  Each one has
	// facing down, up, and left (facing right is just mirrored).  Facing left has two frames
	// of animation, facing up or down animates by simply mirroring the bottom two tiles.  We
	// will store two frames of animation though, because it's simpler and SNES has the VRAM.
	byte[] classTopPalettes    = [0, 1, 2, 0, 3, 2, 0, 0, 2, 0, 0, 2];
	byte[] classBottomPalettes = [0, 2, 1, 0, 3, 1, 0, 0, 1, 3, 0, 1];
	for (int i = 0; i < 12; i++)
	{
		var spriteData4bpp = new byte[0x400]; // 2 rows of SNES sprite data, 32 bytes x 8 sprites x 4 tiles
		int nesOffset = i * 16 * 4 * 4; // 16 bytes, 4 sprites, 4 tiles each
		// Down
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x00, 0x10), new Span<byte>(spriteData4bpp, 0x000, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x10, 0x10), new Span<byte>(spriteData4bpp, 0x020, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x20, 0x10), new Span<byte>(spriteData4bpp, 0x200, 0x20), classBottomPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x30, 0x10), new Span<byte>(spriteData4bpp, 0x220, 0x20), classBottomPalettes[i]);
		// Down second frame, the lower two tiles are horizontally flipped
		FlipNes2bppHorizontally(new Span<byte>(spriteData2bpp, nesOffset + 0x20, 0x10));
		FlipNes2bppHorizontally(new Span<byte>(spriteData2bpp, nesOffset + 0x30, 0x10));
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x00, 0x10), new Span<byte>(spriteData4bpp, 0x040, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x10, 0x10), new Span<byte>(spriteData4bpp, 0x060, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x30, 0x10), new Span<byte>(spriteData4bpp, 0x240, 0x20), classBottomPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x20, 0x10), new Span<byte>(spriteData4bpp, 0x260, 0x20), classBottomPalettes[i]);
		// Up
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x40, 0x10), new Span<byte>(spriteData4bpp, 0x080, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x50, 0x10), new Span<byte>(spriteData4bpp, 0x0A0, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x60, 0x10), new Span<byte>(spriteData4bpp, 0x280, 0x20), classBottomPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x70, 0x10), new Span<byte>(spriteData4bpp, 0x2A0, 0x20), classBottomPalettes[i]);
		// Up second frame
		FlipNes2bppHorizontally(new Span<byte>(spriteData2bpp, nesOffset + 0x60, 0x10));
		FlipNes2bppHorizontally(new Span<byte>(spriteData2bpp, nesOffset + 0x70, 0x10));
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x40, 0x10), new Span<byte>(spriteData4bpp, 0x0C0, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x50, 0x10), new Span<byte>(spriteData4bpp, 0x0E0, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x70, 0x10), new Span<byte>(spriteData4bpp, 0x2C0, 0x20), classBottomPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x60, 0x10), new Span<byte>(spriteData4bpp, 0x2E0, 0x20), classBottomPalettes[i]);
		// Left
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x80, 0x10), new Span<byte>(spriteData4bpp, 0x100, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0x90, 0x10), new Span<byte>(spriteData4bpp, 0x120, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0xA0, 0x10), new Span<byte>(spriteData4bpp, 0x300, 0x20), classBottomPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0xB0, 0x10), new Span<byte>(spriteData4bpp, 0x320, 0x20), classBottomPalettes[i]);
		// Left second frame
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0xC0, 0x10), new Span<byte>(spriteData4bpp, 0x140, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0xD0, 0x10), new Span<byte>(spriteData4bpp, 0x160, 0x20), classTopPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0xE0, 0x10), new Span<byte>(spriteData4bpp, 0x340, 0x20), classBottomPalettes[i]);
		Nes2bppToSnes4bpp(new Span<byte>(spriteData2bpp, nesOffset + 0xF0, 0x10), new Span<byte>(spriteData4bpp, 0x360, 0x20), classBottomPalettes[i]);

		writeStream.Write(spriteData4bpp, 0, spriteData4bpp.Length);
	}
}

/// <summary>
/// Converts a 2bpp 8x8 NES CHR to a 4bpp SNES CHR.
/// </summary>
/// <param name="paletteIndex">A value from 0-3 which will fill in the upper 2 bits of
/// the SNES color.</param>
void Nes2bppToSnes4bpp(Span<byte> nes, Span<byte> snes, byte paletteIndex)
{
	byte lowPalette = (byte)((paletteIndex & 0x01) == 0 ? 0x00 : 0xFF);
	byte highPalette = (byte)((paletteIndex & 0x02) == 0 ? 0x00 : 0xFF);
	for (int y = 0; y < 8; y++) // 8 rows of pixels
	{
		snes[2 * y] = nes[y];
		snes[2 * y + 1] = nes[y + 8];
		snes[2 * y + 16] = lowPalette;
		snes[2 * y + 17] = highPalette;
	}
}

void FlipNes2bppHorizontally(Span<byte> nes)
{
	for (int y = 0; y < 0x10; y++)
	{
		nes[y] = (byte)(
			(nes[y] & 0x01) << 7 |
			(nes[y] & 0x02) << 5 |
			(nes[y] & 0x04) << 3 |
			(nes[y] & 0x08) << 1 |
			(nes[y] & 0x10) >> 1 |
			(nes[y] & 0x20) >> 3 |
			(nes[y] & 0x40) >> 5 |
			(nes[y] & 0x80) >> 7);
	}
}
