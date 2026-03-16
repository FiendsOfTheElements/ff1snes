#!/bin/dotnet run
#:package SixLabors.ImageSharp@3.1.12

using SixLabors.ImageSharp;
using SixLabors.ImageSharp.PixelFormats;

using var input = Image.Load<Bgra5551>("packs/ff4like/CharacterSprites.png");
using var output = new Image<Bgra5551>(input.Width, input.Height);
input.ProcessPixelRows(output, (inputProcess, outputProcess) =>
{
	// 12 character classes
    for (int characterClass = 0; characterClass < 6; characterClass++)
    {
		CopyPixels(input, output, new Rectangle(0, characterClass*40, 208, 24), new Point(0, characterClass*24)); // Battle
		CopyPixels(input, output, new Rectangle(0, characterClass*40 + 24, 208, 16), new Point(0, 192 + characterClass*16)); // Mapman
    }

	// Status ailments
	// Battle
	CopyPixels(input, output, new Rectangle(104, 320, 104, 24), new Point(0, 144));
	CopyPixels(input, output, new Rectangle(104, 240, 104, 24), new Point(104, 144));
	CopyPixels(input, output, new Rectangle(104, 280, 104, 24), new Point(0, 168));
	CopyPixels(input, output, new Rectangle(104, 360, 104, 24), new Point(104, 168));
	// Mapman
	CopyPixels(input, output, new Rectangle(104, 344, 104, 16), new Point(0, 288));
	CopyPixels(input, output, new Rectangle(104, 264, 104, 16), new Point(104, 288));
	CopyPixels(input, output, new Rectangle(104, 304, 104, 16), new Point(0, 304));
	CopyPixels(input, output, new Rectangle(104, 384, 104, 16), new Point(104, 304));
	// Battle add-ons
	CopyPixels(input, output, new Rectangle(0, 288, 104, 80), new Point(104, 320));

	// Vehicles
	CopyPixels(input, output, new Rectangle(0, 240, 104, 48), new Point(0, 320));
});

output.SaveAsPng("test-out.png");

void CopyPixels<T>(Image<T> input, Image<T> output, Rectangle inputArea, Point outputOrigin) where T : unmanaged, IPixel<T>
{
	input.ProcessPixelRows(output, (inputAccessor, outputAccessor) =>
	{
		for (int y = 0; y < inputArea.Height; y++)
		{
			var inputRow = inputAccessor.GetRowSpan(inputArea.Top + y);
			var outputRow = outputAccessor.GetRowSpan(outputOrigin.Y + y);
			for (int x = 0; x < inputArea.Width; x++)
			{
				outputRow[outputOrigin.X + x] = inputRow[inputArea.Left + x];
			}
		}
	});
}
