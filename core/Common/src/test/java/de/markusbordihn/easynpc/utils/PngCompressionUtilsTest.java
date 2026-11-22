/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.utils;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.validator.PngFileValidator;
import de.markusbordihn.easynpc.validator.PngTestImages;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.zip.Deflater;
import javax.imageio.ImageIO;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PngCompressionUtilsTest {

  private static final int WIDTH = 64;
  private static final int HEIGHT = 64;

  private static byte[] gradientImageData() {
    byte[] rawImageData = PngTestImages.rawImageData(WIDTH, HEIGHT);
    int scanlineLength = 1 + WIDTH * 4;
    for (int i = 0; i < rawImageData.length; i++) {
      if (i % scanlineLength != 0) {
        rawImageData[i] = (byte) (i * 7 / 3);
      }
    }
    return rawImageData;
  }

  private static byte[] gradientSkin(int compressionLevel) {
    return PngTestImages.skinWithImageData(
        WIDTH, HEIGHT, PngTestImages.compress(gradientImageData(), compressionLevel));
  }

  private static int[] pixels(byte[] pngBytes) throws IOException {
    BufferedImage image = ImageIO.read(new ByteArrayInputStream(pngBytes));
    return image.getRGB(0, 0, WIDTH, HEIGHT, null, 0, WIDTH);
  }

  private static List<String> chunkTypes(byte[] pngBytes) {
    List<String> chunkTypes = new ArrayList<>();
    int position = 8;
    while (position < pngBytes.length) {
      int chunkLength = ByteBuffer.wrap(pngBytes, position, 4).getInt();
      chunkTypes.add(new String(pngBytes, position + 4, 4, StandardCharsets.US_ASCII));
      position += chunkLength + 12;
    }
    return chunkTypes;
  }

  @Test
  @DisplayName("Stored image data is recompressed into one IDAT without changing pixels")
  void storedImageDataIsRecompressedLosslessly() throws IOException {
    byte[] storedImageData = PngTestImages.compress(gradientImageData(), Deflater.NO_COMPRESSION);
    int splitPosition = storedImageData.length / 2;
    byte[] storedPng =
        PngTestImages.png(
            PngTestImages.chunk("IHDR", PngTestImages.imageHeader(WIDTH, HEIGHT, 0)),
            PngTestImages.chunk("pHYs", new byte[9]),
            PngTestImages.chunk("IDAT", Arrays.copyOfRange(storedImageData, 0, splitPosition)),
            PngTestImages.chunk(
                "IDAT", Arrays.copyOfRange(storedImageData, splitPosition, storedImageData.length)),
            PngTestImages.chunk("tIME", new byte[7]),
            PngTestImages.chunk("IEND", new byte[0]));

    byte[] recompressedPng = PngCompressionUtils.recompress(storedPng, 9);

    assertTrue(recompressedPng.length < storedPng.length / 4);
    assertEquals(List.of("IHDR", "pHYs", "IDAT", "tIME", "IEND"), chunkTypes(recompressedPng));
    PngFileValidator.Result validationResult = PngFileValidator.sanitize(recompressedPng);
    assertTrue(validationResult.isValid(), validationResult.rejectionReason());
    assertTrue(validationResult.removedChunkTypes().isEmpty());
    assertArrayEquals(pixels(storedPng), pixels(recompressedPng));
  }

  @Test
  @DisplayName("Well compressed image keeps its original bytes")
  void wellCompressedImageIsUnchanged() {
    byte[] compressedPng = gradientSkin(Deflater.BEST_COMPRESSION);

    assertSame(compressedPng, PngCompressionUtils.recompress(compressedPng, 9));
  }

  @Test
  @DisplayName("Compression level 0 disables recompression")
  void compressionLevelZeroDisablesRecompression() {
    byte[] storedPng = gradientSkin(Deflater.NO_COMPRESSION);

    assertSame(storedPng, PngCompressionUtils.recompress(storedPng, 0));
  }
}
