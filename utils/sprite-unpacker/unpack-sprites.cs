#!/bin/dotnet run
#:package SixLabors.ImageSharp@3.1.12

using SixLabors.ImageSharp;
using SixLabors.ImageSharp.PixelFormats;

using var image = Image.Load<Bgra5551>("packs/ff4like/CharacterSprites.png");
byte[] sprites = new byte[0x8000]; // a bank full of sprites
for (int c = 0; c < 6; c++) // character classes
{
	var palette = GetPalette(image, new Rectangle(0, 192 + 16*c, 64, 16));
	byte[] character;

	// Top row
	// Facing down
	character = ConvertBgra5551To4bpp(image, new Point(0, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0);
	character = ConvertBgra5551To4bpp(image, new Point(8, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x20);
	character = ConvertBgra5551To4bpp(image, new Point(8, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x40);
	character = ConvertBgra5551To4bpp(image, new Point(0, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x60);
	// Facing up
	character = ConvertBgra5551To4bpp(image, new Point(16, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x80);
	character = ConvertBgra5551To4bpp(image, new Point(24, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0xA0);
	character = ConvertBgra5551To4bpp(image, new Point(24, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0xC0);
	character = ConvertBgra5551To4bpp(image, new Point(16, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0xE0);
	// Facing left
	character = ConvertBgra5551To4bpp(image, new Point(32, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x100);
	character = ConvertBgra5551To4bpp(image, new Point(40, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x120);
	character = ConvertBgra5551To4bpp(image, new Point(48, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x140);
	character = ConvertBgra5551To4bpp(image, new Point(56, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x160);
	// Facing right
	character = ConvertBgra5551To4bpp(image, new Point(40, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x180);
	character = ConvertBgra5551To4bpp(image, new Point(32, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x1A0);
	character = ConvertBgra5551To4bpp(image, new Point(56, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x1C0);
	character = ConvertBgra5551To4bpp(image, new Point(48, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x1E0);

	// Bottom row
	// Facing down
	character = ConvertBgra5551To4bpp(image, new Point(0, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x200);
	character = ConvertBgra5551To4bpp(image, new Point(8, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x220);
	character = ConvertBgra5551To4bpp(image, new Point(8, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x240);
	character = ConvertBgra5551To4bpp(image, new Point(0, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x260);
	// Facing up
	character = ConvertBgra5551To4bpp(image, new Point(16, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x280);
	character = ConvertBgra5551To4bpp(image, new Point(24, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x2A0);
	character = ConvertBgra5551To4bpp(image, new Point(24, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x2C0);
	character = ConvertBgra5551To4bpp(image, new Point(16, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x2E0);
	// Facing left
	character = ConvertBgra5551To4bpp(image, new Point(32, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x300);
	character = ConvertBgra5551To4bpp(image, new Point(40, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x320);
	character = ConvertBgra5551To4bpp(image, new Point(48, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x340);
	character = ConvertBgra5551To4bpp(image, new Point(56, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x400*c + 0x360);
	// Facing right
	character = ConvertBgra5551To4bpp(image, new Point(40, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x380);
	character = ConvertBgra5551To4bpp(image, new Point(32, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x3A0);
	character = ConvertBgra5551To4bpp(image, new Point(56, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x3C0);
	character = ConvertBgra5551To4bpp(image, new Point(48, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x400*c + 0x3E0);
}
for (int c = 0; c < 6; c++) // promoted classes
{
	var palette = GetPalette(image, new Rectangle(104, 192 + 16*c, 64, 16));
	byte[] character;

	// Top row
	// Facing down
	character = ConvertBgra5551To4bpp(image, new Point(104, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0);
	character = ConvertBgra5551To4bpp(image, new Point(112, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x20);
	character = ConvertBgra5551To4bpp(image, new Point(112, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x40);
	character = ConvertBgra5551To4bpp(image, new Point(104, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x60);
	// Facing up
	character = ConvertBgra5551To4bpp(image, new Point(120, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x80);
	character = ConvertBgra5551To4bpp(image, new Point(128, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0xA0);
	character = ConvertBgra5551To4bpp(image, new Point(128, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0xC0);
	character = ConvertBgra5551To4bpp(image, new Point(120, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0xE0);
	// Facing left
	character = ConvertBgra5551To4bpp(image, new Point(136, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x100);
	character = ConvertBgra5551To4bpp(image, new Point(144, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x120);
	character = ConvertBgra5551To4bpp(image, new Point(152, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x140);
	character = ConvertBgra5551To4bpp(image, new Point(160, 192 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x160);
	// Facing right
	character = ConvertBgra5551To4bpp(image, new Point(136, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x180);
	character = ConvertBgra5551To4bpp(image, new Point(144, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x1A0);
	character = ConvertBgra5551To4bpp(image, new Point(152, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x1C0);
	character = ConvertBgra5551To4bpp(image, new Point(160, 192 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x1E0);

	// Bottom row
	// Facing down
	character = ConvertBgra5551To4bpp(image, new Point(104, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x200);
	character = ConvertBgra5551To4bpp(image, new Point(112, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x220);
	character = ConvertBgra5551To4bpp(image, new Point(112, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x240);
	character = ConvertBgra5551To4bpp(image, new Point(104, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x260);
	// Facing up
	character = ConvertBgra5551To4bpp(image, new Point(120, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x280);
	character = ConvertBgra5551To4bpp(image, new Point(128, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x2A0);
	character = ConvertBgra5551To4bpp(image, new Point(128, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x2C0);
	character = ConvertBgra5551To4bpp(image, new Point(120, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x2E0);
	// Facing left
	character = ConvertBgra5551To4bpp(image, new Point(136, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x300);
	character = ConvertBgra5551To4bpp(image, new Point(144, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x320);
	character = ConvertBgra5551To4bpp(image, new Point(152, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x340);
	character = ConvertBgra5551To4bpp(image, new Point(160, 200 + 16*c), palette);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x360);
	// Facing right
	character = ConvertBgra5551To4bpp(image, new Point(136, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x380);
	character = ConvertBgra5551To4bpp(image, new Point(144, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x3A0);
	character = ConvertBgra5551To4bpp(image, new Point(152, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x3C0);
	character = ConvertBgra5551To4bpp(image, new Point(160, 200 + 16*c), palette, flipped: true);
	character.CopyTo(sprites, 0x3000 + 0x400*c + 0x3E0);
}
for (int v = 0; v < 3; v++) // vehicles
{
	var palette = GetPalette(image, new Rectangle(0, 320 + 16*v, 96, 16));
	byte[] character;
	for (int i = 0; i < 12; i++) // Facing up, down, left
	{
		// Top row
		character = ConvertBgra5551To4bpp(image, new Point(i*8, 320 + 16*v), palette);
		character.CopyTo(sprites, 0x6000 + 0x400*v + 0x20*i);
		// Bottom row
		character = ConvertBgra5551To4bpp(image, new Point(i*8, 328 + 16*v), palette);
		character.CopyTo(sprites, 0x6000 + 0x400*v + 0x200 + 0x20*i);
	}
	// Facing right
	// Top row
	character = ConvertBgra5551To4bpp(image, new Point(72, 320 + 16*v), palette);
	character.CopyTo(sprites, 0x6000 + 0x400*v + 0x180);
	character = ConvertBgra5551To4bpp(image, new Point(64, 320 + 16*v), palette);
	character.CopyTo(sprites, 0x6000 + 0x400*v + 0x1A0);
	character = ConvertBgra5551To4bpp(image, new Point(88, 320 + 16*v), palette);
	character.CopyTo(sprites, 0x6000 + 0x400*v + 0x1C0);
	character = ConvertBgra5551To4bpp(image, new Point(80, 320 + 16*v), palette);
	character.CopyTo(sprites, 0x6000 + 0x400*v + 0x1E0);
	// Bottom row
	character = ConvertBgra5551To4bpp(image, new Point(72, 328 + 16*v), palette);
	character.CopyTo(sprites, 0x6000 + 0x400*v + 0x380);
	character = ConvertBgra5551To4bpp(image, new Point(64, 328 + 16*v), palette);
	character.CopyTo(sprites, 0x6000 + 0x400*v + 0x3A0);
	character = ConvertBgra5551To4bpp(image, new Point(88, 328 + 16*v), palette);
	character.CopyTo(sprites, 0x6000 + 0x400*v + 0x3C0);
	character = ConvertBgra5551To4bpp(image, new Point(80, 328 + 16*v), palette);
	character.CopyTo(sprites, 0x6000 + 0x400*v + 0x3E0);
}

Dictionary<ushort, int> GetPalette(Image<Bgra5551> image, Rectangle region)
{
	var palette = new Dictionary<ushort, int>();
	palette.Add(0x7c1f, 0); // color 0 is #FF00FF
	int colorIndex = 0;
	image.ProcessPixelRows(accessor =>
	{
		for (int y = 0; y < region.Height; y++)
		{
			var row = accessor.GetRowSpan(region.Top + y);
			for (int x = 0; x < region.Width; x++)
			{
				var color = (ushort)(row[region.Left + x].PackedValue & 0x7fff);
				if (!palette.ContainsKey(color))
				{
					colorIndex++;
					palette.Add(color, colorIndex);
				}
			}
		}
	});

	return palette;
}

/// <summary>
/// Converts a single 8x8 character from 16-bit to SNES 4bpp.
/// </summary>
/// <param name="characterPos=">The upper-left corner of an 8x8 character to convert.</param>
byte[] ConvertBgra5551To4bpp(Image<Bgra5551> image, Point characterPos, Dictionary<ushort, int> palette, bool flipped = false)
{
	byte[] buffer = new byte[32];
	image.ProcessPixelRows(accessor =>
	{
		for (int y = 0; y < 8; y++)
		{
			var row = accessor.GetRowSpan(characterPos.Y + y);
			for (int x = 0; x < 8; x++)
			{
				var actualX = flipped ? characterPos.X + 7 - x : characterPos.X + x;
				var color = (ushort)(row[actualX].PackedValue & 0x7fff);
				var paletteIndex = palette[color];
				buffer[2*y     ] |= (byte)(((paletteIndex & 0x01)     ) << x);
				buffer[2*y +  1] |= (byte)(((paletteIndex & 0x02) >> 1) << x);
				buffer[2*y + 16] |= (byte)(((paletteIndex & 0x04) >> 2) << x);
				buffer[2*y + 17] |= (byte)(((paletteIndex & 0x08) >> 3) << x);
			}
		}
	});

	return buffer;
}
