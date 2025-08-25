RipOverworldCharacters();
RipOverworldMap();

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
