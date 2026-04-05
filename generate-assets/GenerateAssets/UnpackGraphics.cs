using SixLabors.ImageSharp;
using SixLabors.ImageSharp.PixelFormats;
using System.Runtime.ConstrainedExecution;

namespace GenerateAssets;

using static ImageRipper;

public static class UnpackGraphics
{
	public static void UnpackCharacterSprites()
	{
		using var image = Image.Load<Bgra5551>("packs/ff4like/CharacterSprites.png");
		byte[] sprites = new byte[0x4000]; // half a bank full of sprites
		byte[] palettes = new byte[0x200]; // 16 palettes of 16 colors each, 2 bytes per color
		for (int c = 0; c < 6; c++) // character classes
		{
			var inversePalette = GetInversePalette(image, new Rectangle(0, 192 + 16*c, 64, 16));
			var palette = GetPalette(inversePalette, 16);
			Buffer.BlockCopy(palette, 0, palettes, 32*c, 32);

			byte[] character;
			// Top row
			// Facing down
			character = ConvertBgra5551To4bpp(image, new Point(0, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0);
			character = ConvertBgra5551To4bpp(image, new Point(8, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x20);
			character = ConvertBgra5551To4bpp(image, new Point(0, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x40);
			character = ConvertBgra5551To4bpp(image, new Point(8, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x60);
			// Facing up
			character = ConvertBgra5551To4bpp(image, new Point(16, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x80);
			character = ConvertBgra5551To4bpp(image, new Point(24, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0xA0);
			character = ConvertBgra5551To4bpp(image, new Point(16, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0xC0);
			character = ConvertBgra5551To4bpp(image, new Point(24, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0xE0);
			// Facing left
			character = ConvertBgra5551To4bpp(image, new Point(32, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x100);
			character = ConvertBgra5551To4bpp(image, new Point(40, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x120);
			character = ConvertBgra5551To4bpp(image, new Point(48, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x140);
			character = ConvertBgra5551To4bpp(image, new Point(56, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x160);
			// Facing right
			character = ConvertBgra5551To4bpp(image, new Point(40, 192 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x180);
			character = ConvertBgra5551To4bpp(image, new Point(32, 192 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x1A0);
			character = ConvertBgra5551To4bpp(image, new Point(56, 192 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x1C0);
			character = ConvertBgra5551To4bpp(image, new Point(48, 192 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x1E0);

			// Bottom row
			// Facing down
			character = ConvertBgra5551To4bpp(image, new Point(0, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x200);
			character = ConvertBgra5551To4bpp(image, new Point(8, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x220);
			character = ConvertBgra5551To4bpp(image, new Point(8, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x240);
			character = ConvertBgra5551To4bpp(image, new Point(0, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x260);
			// Facing up
			character = ConvertBgra5551To4bpp(image, new Point(16, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x280);
			character = ConvertBgra5551To4bpp(image, new Point(24, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x2A0);
			character = ConvertBgra5551To4bpp(image, new Point(24, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x2C0);
			character = ConvertBgra5551To4bpp(image, new Point(16, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x2E0);
			// Facing left
			character = ConvertBgra5551To4bpp(image, new Point(32, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x300);
			character = ConvertBgra5551To4bpp(image, new Point(40, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x320);
			character = ConvertBgra5551To4bpp(image, new Point(48, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x340);
			character = ConvertBgra5551To4bpp(image, new Point(56, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x400*c + 0x360);
			// Facing right
			character = ConvertBgra5551To4bpp(image, new Point(40, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x380);
			character = ConvertBgra5551To4bpp(image, new Point(32, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x3A0);
			character = ConvertBgra5551To4bpp(image, new Point(56, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x3C0);
			character = ConvertBgra5551To4bpp(image, new Point(48, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x400*c + 0x3E0);
		}
		for (int c = 0; c < 6; c++) // promoted classes
		{
			var inversePalette = GetInversePalette(image, new Rectangle(104, 192 + 16*c, 64, 16));
			var palette = GetPalette(inversePalette, 16);
			Buffer.BlockCopy(palette, 0, palettes, 32*(c + 6), 32);

			byte[] character;
			// Top row
			// Facing down
			character = ConvertBgra5551To4bpp(image, new Point(104, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0);
			character = ConvertBgra5551To4bpp(image, new Point(112, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x20);
			character = ConvertBgra5551To4bpp(image, new Point(104, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x40);
			character = ConvertBgra5551To4bpp(image, new Point(112, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x60);
			// Facing up
			character = ConvertBgra5551To4bpp(image, new Point(120, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x80);
			character = ConvertBgra5551To4bpp(image, new Point(128, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0xA0);
			character = ConvertBgra5551To4bpp(image, new Point(120, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0xC0);
			character = ConvertBgra5551To4bpp(image, new Point(128, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0xE0);
			// Facing left
			character = ConvertBgra5551To4bpp(image, new Point(136, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x100);
			character = ConvertBgra5551To4bpp(image, new Point(144, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x120);
			character = ConvertBgra5551To4bpp(image, new Point(152, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x140);
			character = ConvertBgra5551To4bpp(image, new Point(160, 192 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x160);
			// Facing right
			character = ConvertBgra5551To4bpp(image, new Point(144, 192 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x180);
			character = ConvertBgra5551To4bpp(image, new Point(136, 192 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x1A0);
			character = ConvertBgra5551To4bpp(image, new Point(160, 192 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x1C0);
			character = ConvertBgra5551To4bpp(image, new Point(152, 192 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x1E0);

			// Bottom row
			// Facing down
			character = ConvertBgra5551To4bpp(image, new Point(104, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x200);
			character = ConvertBgra5551To4bpp(image, new Point(112, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x220);
			character = ConvertBgra5551To4bpp(image, new Point(112, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x240);
			character = ConvertBgra5551To4bpp(image, new Point(104, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x260);
			// Facing up
			character = ConvertBgra5551To4bpp(image, new Point(120, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x280);
			character = ConvertBgra5551To4bpp(image, new Point(128, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x2A0);
			character = ConvertBgra5551To4bpp(image, new Point(128, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x2C0);
			character = ConvertBgra5551To4bpp(image, new Point(120, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x2E0);
			// Facing left
			character = ConvertBgra5551To4bpp(image, new Point(136, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x300);
			character = ConvertBgra5551To4bpp(image, new Point(144, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x320);
			character = ConvertBgra5551To4bpp(image, new Point(152, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x340);
			character = ConvertBgra5551To4bpp(image, new Point(160, 200 + 16*c), inversePalette);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x360);
			// Facing right
			character = ConvertBgra5551To4bpp(image, new Point(144, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x380);
			character = ConvertBgra5551To4bpp(image, new Point(136, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x3A0);
			character = ConvertBgra5551To4bpp(image, new Point(160, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x3C0);
			character = ConvertBgra5551To4bpp(image, new Point(152, 200 + 16*c), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x1800 + 0x400*c + 0x3E0);
		}
		for (int v = 0; v < 3; v++) // vehicles
		{
			var inversePalette = GetInversePalette(image, new Rectangle(0, 320 + 16*v, 96, 16));
			var palette = GetPalette(inversePalette, 16);
			Buffer.BlockCopy(palette, 0, palettes, 32*(v + 12), 32);

			byte[] character;
			for (int i = 0; i < 12; i++) // Facing up, down, left
			{
				// Top row
				character = ConvertBgra5551To4bpp(image, new Point(i*8, 320 + 16*v), inversePalette);
				character.CopyTo(sprites, 0x3000 + 0x400*v + 0x20*i);
				// Bottom row
				character = ConvertBgra5551To4bpp(image, new Point(i*8, 328 + 16*v), inversePalette);
				character.CopyTo(sprites, 0x3000 + 0x400*v + 0x200 + 0x20*i);
			}
			// Facing right
			// Top row
			character = ConvertBgra5551To4bpp(image, new Point(72, 320 + 16*v), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x3000 + 0x400*v + 0x180);
			character = ConvertBgra5551To4bpp(image, new Point(64, 320 + 16*v), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x3000 + 0x400*v + 0x1A0);
			character = ConvertBgra5551To4bpp(image, new Point(88, 320 + 16*v), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x3000 + 0x400*v + 0x1C0);
			character = ConvertBgra5551To4bpp(image, new Point(80, 320 + 16*v), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x3000 + 0x400*v + 0x1E0);
			// Bottom row
			character = ConvertBgra5551To4bpp(image, new Point(72, 328 + 16*v), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x3000 + 0x400*v + 0x380);
			character = ConvertBgra5551To4bpp(image, new Point(64, 328 + 16*v), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x3000 + 0x400*v + 0x3A0);
			character = ConvertBgra5551To4bpp(image, new Point(88, 328 + 16*v), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x3000 + 0x400*v + 0x3C0);
			character = ConvertBgra5551To4bpp(image, new Point(80, 328 + 16*v), inversePalette, flipped: true);
			character.CopyTo(sprites, 0x3000 + 0x400*v + 0x3E0);
		}
		// Airship shadow and mini ship
		byte[] etcCharacter = new byte[32];
		var etcInversePalette = GetInversePalette(image, new Rectangle(0, 320, 96, 16)); // same as vehicle palette

		etcCharacter = ConvertBgra5551To4bpp(image, new Point(64, 272), etcInversePalette);
		etcCharacter.CopyTo(sprites, 0x3c00);
		etcCharacter = ConvertBgra5551To4bpp(image, new Point(72, 272), etcInversePalette);
		etcCharacter.CopyTo(sprites, 0x3c20);
		etcCharacter = ConvertBgra5551To4bpp(image, new Point(80, 272), etcInversePalette);
		etcCharacter.CopyTo(sprites, 0x3c40);
		etcCharacter = ConvertBgra5551To4bpp(image, new Point(88, 272), etcInversePalette);
		etcCharacter.CopyTo(sprites, 0x3c60);
		etcCharacter = ConvertBgra5551To4bpp(image, new Point(64, 280), etcInversePalette);
		etcCharacter.CopyTo(sprites, 0x3d00);
		etcCharacter = ConvertBgra5551To4bpp(image, new Point(72, 280), etcInversePalette);
		etcCharacter.CopyTo(sprites, 0x3d20);
		etcCharacter = ConvertBgra5551To4bpp(image, new Point(80, 280), etcInversePalette);
		etcCharacter.CopyTo(sprites, 0x3d40);
		etcCharacter = ConvertBgra5551To4bpp(image, new Point(88, 280), etcInversePalette);
		etcCharacter.CopyTo(sprites, 0x3d60);

		using var spriteFile = File.OpenWrite("assets/graphics/overworld-sprites.4bpp");
		spriteFile.Write(sprites);
		spriteFile.Close();

		using var paletteFile = File.OpenWrite("assets/graphics/overworld-sprite-palettes.pal");
		paletteFile.Write(palettes);
		paletteFile.Close();
	}

	public static void UnpackOverworldGraphics()
	{
		using var image = Image.Load<Bgra5551>("packs/ff4like/OverworldTiles.png");
		var inversePalette = GetInversePalette(image, new Rectangle(0, 0, 144, 240));
		var palette = GetPalette(inversePalette, 128);

		// We need to generate tilemaps and the character graphics themselves.
		// For each map tile in the original game, there is a 16x16 section of the graphics pack
		// that has the graphics for that tile.  So the graphicsMap maps which graphics belong to
		// which original map tile.
		(int x, int y)[] graphicsMap = [
			(  0,   0), ( 32, 144), ( 48, 144), (  0,  16), ( 16,  16), ( 32,  16), ( 96,  16), (112,  16), (128,  16), ( 32, 112), ( 48, 112), (  0, 144), ( 16, 144), ( 32, 160), ( 32,   0), ( 96, 144),
			(  0,  64), ( 16,  64), ( 32,  64), (  0,  32), ( 16,  32), ( 32,  32), ( 96,  32), (112,  32), (128,  32), ( 32, 128), ( 48, 128), (  0, 160), ( 16, 160), ( 32, 192), ( 48, 160), (128, 128),
			(  0,  80), ( 16,  80), ( 32,  80), (  0,  48), ( 16,  48), ( 32,  48), ( 96,  48), (112,  48), (128,  48), (  0, 160), ( 16, 160), ( 32,   0), ( 64, 160), ( 80, 160), ( 96, 160), ( 32,   0),
			(  0,  96), ( 16,  96), ( 32,   0), ( 32,  96), ( 32,   0), ( 32,   0), (112,  80), (112,  80), (  0, 160), ( 16, 160), ( 32,   0), (112, 160), (  0,   0), (  0,   0), (  0,   0), (112, 176),
			( 48,  16), ( 80,  16), ( 96,  64), (128,  64), ( 64,  32), (112,  80), ( 48,   0), ( 64, 112), ( 80, 112), (128, 160), (128, 160), (112, 160), (128, 160), (128, 176), (128, 160), (112, 176),
			( 48,  48), ( 80,  48), ( 96,  96), (128,  96), ( 64,  80), (128, 112), ( 64, 144), ( 64, 128), ( 80, 128), ( 80, 144), (128, 176), (112, 160), ( 48, 176), (128, 176), ( 96, 176), (112, 176),
			( 48,  64), ( 80,  64), ( 96, 112), (112, 112), (  0, 112), ( 16, 112), ( 16,   0), ( 16,   0), ( 16,   0), ( 16,   0), ( 16,   0), (112, 160), ( 16,   0), (128, 160), ( 16,   0), (112, 176),
			( 48,  96), ( 80,  96), ( 96, 128), (112, 128), (  0, 128), ( 16, 128), (  0,   0), (  0,   0), (128, 144), (  0,   0), (  0,   0), ( 48, 176), ( 80, 160), ( 64, 176), ( 80, 176), ( 96, 176),
		];
		Console.WriteLine($"{graphicsMap.Distinct().Count()} unique map tiles");

		// The tileMap stores which 8x8 characters make up each tile.  Tiles can be flipped horizontally
		// or vertically, so we need to compare to flipped versions of tiles we've seen before.
		var ulTilemap = new byte[128];
		var urTilemap = new byte[128];
		var llTilemap = new byte[128];
		var lrTilemap = new byte[128];
		Dictionary<int, int> seenChrLookup = [];
		var chrGraphics = new byte[0x8000]; // This only needs to be 0x4000, but we make it big enough to hold
		// all the possible graphics that could be in the file.  That way this doesn't crash, and you can examine
		// its output to see what needs to be trimmed.
		for (int tileIndex = 0; tileIndex < 128; tileIndex++)
		{
			byte[] chr;
			int hash;
			var (x, y) = graphicsMap[tileIndex];
			chr = ConvertBgra5551To8bpp(image, new Point(x, y), inversePalette);
			hash = HashChr(chr);
			if (!seenChrLookup.ContainsKey(hash))
			{
				int thisChrIndex = seenChrLookup.Count;
				chr.CopyTo(chrGraphics, 64*thisChrIndex);
				seenChrLookup.Add(hash, thisChrIndex);
			}
			ulTilemap[tileIndex] = (byte)seenChrLookup[hash];

			chr = ConvertBgra5551To8bpp(image, new Point(x + 8, y), inversePalette);
			hash = HashChr(chr);
			if (!seenChrLookup.ContainsKey(hash))
			{
				chr.CopyTo(chrGraphics, 64*seenChrLookup.Count);
				seenChrLookup.Add(hash, seenChrLookup.Count);
			}
			urTilemap[tileIndex] = (byte)seenChrLookup[hash];

			chr = ConvertBgra5551To8bpp(image, new Point(x, y + 8), inversePalette);
			hash = HashChr(chr);
			if (!seenChrLookup.ContainsKey(hash))
			{
				chr.CopyTo(chrGraphics, 64*seenChrLookup.Count);
				seenChrLookup.Add(hash, seenChrLookup.Count);
			}
			llTilemap[tileIndex] = (byte)seenChrLookup[hash];

			chr = ConvertBgra5551To8bpp(image, new Point(x + 8, y + 8), inversePalette);
			hash = HashChr(chr);
			if (!seenChrLookup.ContainsKey(hash))
			{
				chr.CopyTo(chrGraphics, 64*seenChrLookup.Count);
				seenChrLookup.Add(hash, seenChrLookup.Count);
			}
			lrTilemap[tileIndex] = (byte)seenChrLookup[hash];
		}

		Console.WriteLine($"{seenChrLookup.Count} unique chr");

		using var paletteFile = File.OpenWrite("assets/graphics/overworld-map-palette.pal");
		var paletteBytes = new byte[128];
		Buffer.BlockCopy(palette, 0, paletteBytes, 0, 128);
		paletteFile.Write(paletteBytes);
		paletteFile.Close();

		using var overworldChrFile = File.OpenWrite("assets/graphics/overworld-chr.m7");
		overworldChrFile.Write(chrGraphics, 0, 0x4000); // only write 16 KB, if there's more, we need to strip down
		overworldChrFile.Close();

		using var overworldTilemapFile = File.OpenWrite("assets/data/overworld-tilemaps.bin");
		overworldTilemapFile.Write(ulTilemap);
		overworldTilemapFile.Write(urTilemap);
		overworldTilemapFile.Write(llTilemap);
		overworldTilemapFile.Write(lrTilemap);
		overworldTilemapFile.Close();
	}

	public static void UnpackTitleScreen()
	{
		using var image = Image.Load<Bgra5551>("packs/ff4like/TitleScreen.png");
		var inversePalette = GetInversePalette(image, new Rectangle(0, 0, 256, 240));
		var palette = GetPalette(inversePalette, 16);

		byte[] spriteGraphics = new byte[0x2000]; // quarter bank full of sprites

		Rip16x16Sprite(image, new Point(2, 56), 15, inversePalette, spriteGraphics, 0); // Top of F
		Rip16x16Sprite(image, new Point(2, 72), 15, inversePalette, spriteGraphics, 1); // Middle of F
		Rip16x16Sprite(image, new Point(2, 88), 15, inversePalette, spriteGraphics, 2); // Bottom of F (or I/T/Y)

		Rip16x16Sprite(image, new Point(18, 56), 10, inversePalette, spriteGraphics, 3); // Top of I (or L)
		Rip16x16Sprite(image, new Point(18, 72), 10, inversePalette, spriteGraphics, 4); // Middle of I (or T)

		Rip16x16Sprite(image, new Point(29, 56), 16, inversePalette, spriteGraphics, 5); // Top of N
		Rip16x16Sprite(image, new Point(45, 56), 2, inversePalette, spriteGraphics, 6); // Top-right of N
		Rip16x16Sprite(image, new Point(29, 72), 16, inversePalette, spriteGraphics, 7); // Middle of N
		Rip16x16Sprite(image, new Point(29, 88), 16, inversePalette, spriteGraphics, 8); // Bottom of N
		Rip16x16Sprite(image, new Point(45, 88), 2, inversePalette, spriteGraphics, 9); // Bottom-right of N

		Rip16x16Sprite(image, new Point(49, 56), 16, inversePalette, spriteGraphics, 10); // Top of A
		Rip16x16Sprite(image, new Point(49, 72), 16, inversePalette, spriteGraphics, 11); // Middle of A
		Rip16x16Sprite(image, new Point(49, 88), 16, inversePalette, spriteGraphics, 12); // Bottom of A
		Rip16x16Sprite(image, new Point(65, 88), 3, inversePalette, spriteGraphics, 13); // Bottom-right of A

		Rip16x16Sprite(image, new Point(68, 72), 15, inversePalette, spriteGraphics, 14); // Middle of L
		Rip16x16Sprite(image, new Point(68, 88), 15, inversePalette, spriteGraphics, 15); // Bottom of L

		Rip16x16Sprite(image, new Point(186, 56), 14, inversePalette, spriteGraphics, 16); // Top of T

		Rip16x16Sprite(image, new Point(221, 56), 13, inversePalette, spriteGraphics, 17); // Top of S
		Rip16x16Sprite(image, new Point(221, 72), 13, inversePalette, spriteGraphics, 18); // Middle of S
		Rip16x16Sprite(image, new Point(221, 88), 13, inversePalette, spriteGraphics, 19); // Bottom of S

		Rip16x16Sprite(image, new Point(235, 56), 16, inversePalette, spriteGraphics, 20); // Top of Y
		Rip16x16Sprite(image, new Point(251, 56), 3, inversePalette, spriteGraphics, 21); // Top-right of Y
		Rip16x16Sprite(image, new Point(235, 72), 16,inversePalette, spriteGraphics, 22); // Middle of Y

		Rip32x32Sprite(image, new Point(93, 52), 29, inversePalette, spriteGraphics, 8); // Top of crystal
		Rip32x32Sprite(image, new Point(93, 84), 29, inversePalette, spriteGraphics, 9); // Bottom of crystal

		Rip32x32Sprite(image, new Point(130, 107), 32, inversePalette, spriteGraphics, 10); // Superizer
		Rip32x32Sprite(image, new Point(162, 107), 32, inversePalette, spriteGraphics, 11); // Superizer
		Rip32x32Sprite(image, new Point(194, 107), 32, inversePalette, spriteGraphics, 12); // Superizer
		Rip32x32Sprite(image, new Point(226, 107), 27, inversePalette, spriteGraphics, 13); // Superizer

		byte[] spriteData = new byte[544];

		WriteSpriteData(spriteData, 0, new Point(2, 56), 0, isLarge: false); // Top of F
		WriteSpriteData(spriteData, 1, new Point(2, 72), 2, isLarge: false); // Middle of F
		WriteSpriteData(spriteData, 2, new Point(2, 88), 4, isLarge: false); // Bottom of F

		WriteSpriteData(spriteData, 3, new Point(18, 56), 6, isLarge: false); // Top of I
		WriteSpriteData(spriteData, 4, new Point(18, 72), 8, isLarge: false); // Middle of I
		WriteSpriteData(spriteData, 5, new Point(18, 88), 4, isLarge: false); // Bottom of I

		WriteSpriteData(spriteData, 6, new Point(28, 56), 10, isLarge: false); // Top of N
		WriteSpriteData(spriteData, 7, new Point(44, 56), 12, isLarge: false); // Top-right of N
		WriteSpriteData(spriteData, 8, new Point(28, 72), 14, isLarge: false); // Middle of N
		WriteSpriteData(spriteData, 9, new Point(28, 88), 32, isLarge: false); // Bottom of N
		WriteSpriteData(spriteData, 10, new Point(44, 88), 34, isLarge: false); // Bottom-right of N

		WriteSpriteData(spriteData, 11, new Point(48, 56), 36, isLarge: false); // Top of A
		WriteSpriteData(spriteData, 12, new Point(48, 72), 38, isLarge: false); // Middle of A
		WriteSpriteData(spriteData, 13, new Point(48, 88), 40, isLarge: false); // Bottom of A
		WriteSpriteData(spriteData, 14, new Point(64, 88), 42, isLarge: false); // Bottom-right of A
	
		WriteSpriteData(spriteData, 15, new Point(68, 56), 12, isLarge: false); // Top of L
		WriteSpriteData(spriteData, 16, new Point(68, 72), 44, isLarge: false); // Middle of L
		WriteSpriteData(spriteData, 17, new Point(68, 88), 46, isLarge: false); // Bottom of L

		WriteSpriteData(spriteData, 18, new Point(131, 56), 0, isLarge: false); // Top of F
		WriteSpriteData(spriteData, 19, new Point(131, 72), 2, isLarge: false); // Middle of F
		WriteSpriteData(spriteData, 20, new Point(131, 88), 4, isLarge: false); // Bottom of F

		WriteSpriteData(spriteData, 21, new Point(147, 56), 36, isLarge: false); // Top of A
		WriteSpriteData(spriteData, 22, new Point(147, 72), 38, isLarge: false); // Middle of A
		WriteSpriteData(spriteData, 23, new Point(147, 88), 40, isLarge: false); // Bottom of A
		WriteSpriteData(spriteData, 24, new Point(163, 88), 42, isLarge: false); // Bottom-right of A

		WriteSpriteData(spriteData, 25, new Point(167, 56), 10, isLarge: false); // Top of N
		WriteSpriteData(spriteData, 26, new Point(183, 56), 12, isLarge: false); // Top-right of N
		WriteSpriteData(spriteData, 27, new Point(167, 72), 14, isLarge: false); // Middle of N
		WriteSpriteData(spriteData, 28, new Point(167, 88), 32, isLarge: false); // Bottom of N
		WriteSpriteData(spriteData, 29, new Point(183, 88), 34, isLarge: false); // Bottom-right of N

		WriteSpriteData(spriteData, 30, new Point(186, 56), 64, isLarge: false); // Top of T
		WriteSpriteData(spriteData, 31, new Point(186, 72), 8, isLarge: false); // Middle of T
		WriteSpriteData(spriteData, 32, new Point(186, 88), 4, isLarge: false); // Bottom of T

		WriteSpriteData(spriteData, 33, new Point(201, 56), 36, isLarge: false); // Top of A
		WriteSpriteData(spriteData, 34, new Point(201, 72), 38, isLarge: false); // Middle of A
		WriteSpriteData(spriteData, 35, new Point(201, 88), 40, isLarge: false); // Bottom of A
		WriteSpriteData(spriteData, 36, new Point(217, 88), 42, isLarge: false); // Bottom-right of A

		WriteSpriteData(spriteData, 37, new Point(221, 56), 66, isLarge: false); // Top of S
		WriteSpriteData(spriteData, 38, new Point(221, 72), 68, isLarge: false); // Middle of S
		WriteSpriteData(spriteData, 39, new Point(221, 88), 70, isLarge: false); // Bottom of S

		WriteSpriteData(spriteData, 40, new Point(235, 56), 72, isLarge: false); // Top of Y
		WriteSpriteData(spriteData, 41, new Point(251, 56), 74, isLarge: false); // Top-right of Y
		WriteSpriteData(spriteData, 42, new Point(235, 72), 76, isLarge: false); // Middle of Y
		WriteSpriteData(spriteData, 43, new Point(235, 88), 4, isLarge: false); // Bottom of Y

		WriteSpriteData(spriteData, 44, new Point(93, 52), 128, isLarge: false); // Top of Crystal
		WriteSpriteData(spriteData, 45, new Point(93, 84), 132, isLarge: false); // Bottom of Crystal

		WriteSpriteData(spriteData, 46, new Point(131, 107), 136, isLarge: false); // Superizer
		WriteSpriteData(spriteData, 47, new Point(163, 107), 140, isLarge: false); // Superizer
		WriteSpriteData(spriteData, 48, new Point(195, 107), 192, isLarge: false); // Superizer
		WriteSpriteData(spriteData, 49, new Point(227, 107), 196, isLarge: false); // Superizer

		using var paletteFile = File.OpenWrite("assets/graphics/title-screen-palette.pal");
		var paletteBytes = new byte[32];
		Buffer.BlockCopy(palette, 0, paletteBytes, 0, 32);
		paletteFile.Write(paletteBytes);
		paletteFile.Close();

		using var spriteGraphicsFile = File.OpenWrite("assets/graphics/title-screen-sprites.4bpp");
		spriteGraphicsFile.Write(spriteGraphics);
		spriteGraphicsFile.Close();

		using var spriteDataFile = File.OpenWrite("assets/data/title-screen-sprites.bin");
		spriteDataFile.Write(spriteData);
		spriteDataFile.Close();
	}

	private static void Rip16x16Sprite(Image<Bgra5551> image, Point origin, int width, Dictionary<ushort, int> inversePalette, byte[] buffer, int spriteIndex)
	{
		byte[] chr;
		int row = spriteIndex / 8;
		int col = spriteIndex % 8;
		int offset = (row * 16 * 2 + col * 2) * 32;
		chr = ConvertBgra5551To4bpp(image, origin, inversePalette, limit: width);
		chr.CopyTo(buffer, offset);
		chr = ConvertBgra5551To4bpp(image, origin + new Size(8, 0), inversePalette, limit: width - 8);
		chr.CopyTo(buffer, offset + 32);
		chr = ConvertBgra5551To4bpp(image, origin + new Size(0, 8), inversePalette, limit: width);
		chr.CopyTo(buffer, offset + 16 * 32);
		chr = ConvertBgra5551To4bpp(image, origin + new Size(8, 8), inversePalette, limit: width - 8);
		chr.CopyTo(buffer, offset + 16 * 32 + 32);
	}

	private static void Rip32x32Sprite(Image<Bgra5551> image, Point origin, int width, Dictionary<ushort, int> inversePalette, byte[] buffer, int spriteIndex)
	{
		byte[] chr;
		int row = spriteIndex / 4;
		int col = spriteIndex % 4;
		int offset = (row * 16 * 4 + col * 4) * 32;
		for (int j = 0; j < 4; j++)
		{
			for (int i = 0; i < 4; i++)
			{
				chr = ConvertBgra5551To4bpp(image, origin + new Size(8 * i, 8 * j), inversePalette, limit: width - i * 8);
				chr.CopyTo(buffer, offset + j * 16 * 32 + i * 32);
			}
		}
	}

	private static void WriteSpriteData(byte[] spriteData, int spriteIndex, Point position, int spriteTile, bool isLarge)
	{
		spriteData[spriteIndex * 4] = (byte)(position.X % 256);
		spriteData[spriteIndex * 4 + 1] = (byte)(position.Y);
		spriteData[spriteIndex * 4 + 2] = (byte)(spriteTile % 256);
		spriteData[spriteIndex * 4 + 3] = (byte)(spriteTile / 256);

		int highIndex = 512 + spriteIndex / 4;
		int shift = (spriteIndex % 4) * 2;
		byte mask = (byte)(0b00000011 << shift);
		byte data = (byte)(position.X / 512 | (isLarge ? 0b00000010 : 0));

		spriteData[highIndex] = (byte)((spriteData[highIndex] & ~mask) | (data << shift));
	}
}
