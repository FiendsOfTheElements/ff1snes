RipOverworldCharacters();
RipOverworldTilemaps();
RipOverworldTilePaletteMaps();
RipOverworldMap();

void RipOverworldCharacters()
{
	using var readStream = File.Open("ff1.nes", FileMode.Open, FileAccess.Read);
	using var writeStream = File.Open("overworld-chr.4bpp", FileMode.Create, FileAccess.Write);

	// Because of how bitplanes work, if we read a 2bpp character into the space for a 4bpp character,
	// the remaining bitplanes will just be zeroed out, so it will be a valid 4bpp character.
	var tileData4bpp = new byte[0x20]; // one 4bpp tile is 32 bytes
	readStream.Position = 0x8000; // Overworld character data starts at the beginning of bank 02
	for (int i = 0; i < 0x100; i++) // 256 tiles
	{
		readStream.Read(tileData4bpp, 0, 0x10); // Read one 2bpp character (16 bytes)
		writeStream.Write(tileData4bpp, 0, 0x20); // Write one 4bpp character (32 bytes)
	}

	readStream.Close();
	writeStream.Close();
}

void RipOverworldTilemaps()
{
	using var readStream = File.Open("ff1.nes", FileMode.Open, FileAccess.Read);
	using var writeStream = File.Open("overworld-tilemaps.bin", FileMode.Create, FileAccess.Write);

	// There are 128 tiles.  The tilemap stores the upper-left character of each tile, then the upper-right, lower-left, and lower-right.
	// A character index is one byte, so the whole tilemap is 512 bytes.
	var tilemapData = new byte[0x200];
	readStream.Position = 0x0100; // Overworld tilemap data starts at 0x0100 in bank 00
	readStream.Read(tilemapData, 0, tilemapData.Length); // Read the entire tilemap
	writeStream.Write(tilemapData, 0, tilemapData.Length); // Write the tilemap to file

	readStream.Close();
	writeStream.Close();
}

void RipOverworldTilePaletteMaps()
{
	using var readStream = File.Open("ff1.nes", FileMode.Open, FileAccess.Read);
	using var writeStream = File.Open("overworld-tile-palette-maps.bin", FileMode.Create, FileAccess.Write);

	// There are 128 tiles, each made of 4 characters, and each character has a palette.
	// A palette index is only 2 bits, so this data fits into 128 bytes.
	var paletteMapData = new byte[0x80];
	readStream.Position = 0x0300; // Overworld pallette map data starts at 0x0300 in bank 00
	readStream.Read(paletteMapData, 0, paletteMapData.Length); // Read the entire palette map
	writeStream.Write(paletteMapData, 0, paletteMapData.Length); // Write the palette map to file

	readStream.Close();
	writeStream.Close();
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
	readStream.Position = 0x4000; // Overworld map data starts at the beginning of bank 01
	readStream.Read(mapData, 0, mapData.Length); // Read the entire map, including the pointers
	writeStream.Write(mapData, 0, mapData.Length); // Write the map to file

	readStream.Close();
	writeStream.Close();
}
