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

package de.markusbordihn.easynpc.validator;

import static de.markusbordihn.easynpc.validator.PngTestImages.chunk;
import static de.markusbordihn.easynpc.validator.PngTestImages.compress;
import static de.markusbordihn.easynpc.validator.PngTestImages.imageHeader;
import static de.markusbordihn.easynpc.validator.PngTestImages.png;
import static de.markusbordihn.easynpc.validator.PngTestImages.rawImageData;
import static de.markusbordihn.easynpc.validator.PngTestImages.skinWithImageData;
import static de.markusbordihn.easynpc.validator.PngTestImages.validSkin;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class PngFileValidatorTest {

  @ParameterizedTest
  @CsvSource({"64,64", "64,32", "128,128", "48,32"})
  @DisplayName("Valid skins are returned unchanged")
  void validSkinsAreReturnedUnchanged(int width, int height) {
    byte[] skin = validSkin(width, height);

    PngFileValidator.Result result = PngFileValidator.sanitize(skin);

    assertTrue(result.isValid(), result.rejectionReason());
    assertSame(skin, result.pngBytes());
    assertTrue(result.removedChunkTypes().isEmpty());
  }

  @Test
  @DisplayName("Interlaced skin is valid")
  void interlacedSkinIsValid() {
    ByteArrayOutputStream interlacedData = new ByteArrayOutputStream();
    int[][] passes = {
      {0, 0, 8, 8},
      {4, 0, 8, 8},
      {0, 4, 4, 8},
      {2, 0, 4, 4},
      {0, 2, 2, 4},
      {1, 0, 2, 2},
      {0, 1, 1, 2}
    };
    for (int[] pass : passes) {
      int passWidth = (64 - pass[0] + pass[2] - 1) / pass[2];
      int passHeight = (64 - pass[1] + pass[3] - 1) / pass[3];
      interlacedData.writeBytes(rawImageData(passWidth, passHeight));
    }
    byte[] skin =
        png(
            chunk("IHDR", imageHeader(64, 64, 1)),
            chunk("IDAT", compress(interlacedData.toByteArray())),
            chunk("IEND", new byte[0]));

    assertTrue(PngFileValidator.sanitize(skin).isValid());
  }

  @Test
  @DisplayName("Metadata chunks are removed and pixel data stays identical")
  void metadataChunksAreRemoved() {
    byte[] imageData = compress(rawImageData(64, 64));
    byte[] skin =
        png(
            chunk("IHDR", imageHeader(64, 64, 0)),
            chunk("tEXt", "Comment\0<script>alert(1)</script>".getBytes(StandardCharsets.UTF_8)),
            chunk("iCCP", new byte[] {'p', 0, 0, 1, 2, 3}),
            chunk("pHYs", new byte[9]),
            chunk("IDAT", imageData),
            chunk("zzZz", new byte[] {1, 2, 3}),
            chunk("IEND", new byte[0]));

    PngFileValidator.Result result = PngFileValidator.sanitize(skin);

    assertTrue(result.isValid(), result.rejectionReason());
    assertEquals(List.of("tEXt", "iCCP", "zzZz"), result.removedChunkTypes());
    assertArrayEquals(
        png(
            chunk("IHDR", imageHeader(64, 64, 0)),
            chunk("pHYs", new byte[9]),
            chunk("IDAT", imageData),
            chunk("IEND", new byte[0])),
        result.pngBytes());
  }

  @Test
  @DisplayName("Data after IEND is rejected")
  void dataAfterEndIsRejected() {
    byte[] skin = validSkin(64, 64);
    byte[] polyglot = Arrays.copyOf(skin, skin.length + 4);
    polyglot[skin.length] = 'P';
    polyglot[skin.length + 1] = 'K';

    assertFalse(PngFileValidator.sanitize(polyglot).isValid());
  }

  @Test
  @DisplayName("CRC mismatch is rejected")
  void crcMismatchIsRejected() {
    byte[] skin = validSkin(64, 64);
    skin[skin.length - 13] ^= 0x01;

    assertFalse(PngFileValidator.sanitize(skin).isValid());
  }

  @Test
  @DisplayName("Unknown critical chunk is rejected")
  void unknownCriticalChunkIsRejected() {
    byte[] skin =
        png(
            chunk("IHDR", imageHeader(64, 64, 0)),
            chunk("ABCD", new byte[] {1}),
            chunk("IDAT", compress(rawImageData(64, 64))),
            chunk("IEND", new byte[0]));

    assertFalse(PngFileValidator.sanitize(skin).isValid());
  }

  @ParameterizedTest
  @CsvSource({"16384,16384", "4096,64", "100,100", "16,16", "0,0"})
  @DisplayName("Unsupported image sizes are rejected before decoding")
  void unsupportedImageSizesAreRejected(int width, int height) {
    byte[] skin = skinWithImageData(width, height, compress(new byte[16]));

    assertFalse(PngFileValidator.sanitize(skin).isValid());
  }

  @Test
  @DisplayName("Compression bomb is rejected")
  void compressionBombIsRejected() {
    byte[] skin = skinWithImageData(64, 64, compress(new byte[64 * 1024 * 1024]));

    assertFalse(PngFileValidator.sanitize(skin).isValid());
  }

  @Test
  @DisplayName("Too little image data is rejected")
  void tooLittleImageDataIsRejected() {
    byte[] skin = skinWithImageData(64, 64, compress(rawImageData(64, 32)));

    assertFalse(PngFileValidator.sanitize(skin).isValid());
  }

  @Test
  @DisplayName("Data after the image data stream is rejected")
  void dataAfterImageDataStreamIsRejected() {
    byte[] imageData = compress(rawImageData(64, 64));
    byte[] imageDataWithPayload = Arrays.copyOf(imageData, imageData.length + 8);
    byte[] skin = skinWithImageData(64, 64, imageDataWithPayload);

    assertFalse(PngFileValidator.sanitize(skin).isValid());
  }

  @Test
  @DisplayName("Split IDAT chunks must be consecutive")
  void splitImageDataMustBeConsecutive() {
    byte[] imageData = compress(rawImageData(64, 64));
    byte[] firstPart = Arrays.copyOfRange(imageData, 0, imageData.length / 2);
    byte[] secondPart = Arrays.copyOfRange(imageData, imageData.length / 2, imageData.length);

    byte[] consecutiveSkin =
        png(
            chunk("IHDR", imageHeader(64, 64, 0)),
            chunk("IDAT", firstPart),
            chunk("IDAT", secondPart),
            chunk("IEND", new byte[0]));
    byte[] splitSkin =
        png(
            chunk("IHDR", imageHeader(64, 64, 0)),
            chunk("IDAT", firstPart),
            chunk("tIME", new byte[7]),
            chunk("IDAT", secondPart),
            chunk("IEND", new byte[0]));

    assertTrue(PngFileValidator.sanitize(consecutiveSkin).isValid());
    assertFalse(PngFileValidator.sanitize(splitSkin).isValid());
  }

  @Test
  @DisplayName("Missing signature, header first and too large files are rejected")
  void malformedFilesAreRejected() {
    byte[] skin = validSkin(64, 64);
    byte[] withoutSignature = Arrays.copyOfRange(skin, 8, skin.length);
    byte[] withoutHeader =
        png(chunk("IDAT", compress(rawImageData(64, 64))), chunk("IEND", new byte[0]));
    byte[] tooLarge = Arrays.copyOf(skin, PngFileValidator.MAX_FILE_SIZE + 1);
    byte[] truncated = Arrays.copyOf(skin, skin.length - 5);

    assertFalse(PngFileValidator.sanitize(null).isValid());
    assertFalse(PngFileValidator.sanitize(withoutSignature).isValid());
    assertFalse(PngFileValidator.sanitize(withoutHeader).isValid());
    assertFalse(PngFileValidator.sanitize(tooLarge).isValid());
    assertFalse(PngFileValidator.sanitize(truncated).isValid());
  }
}
