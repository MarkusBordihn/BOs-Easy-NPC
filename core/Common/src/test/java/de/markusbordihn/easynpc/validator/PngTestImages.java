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

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.zip.CRC32;
import java.util.zip.Deflater;

public final class PngTestImages {

  private static final byte[] PNG_SIGNATURE = {
    (byte) 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A
  };
  private static final int RGBA_COLOR_TYPE = 6;

  private PngTestImages() {}

  public static byte[] imageHeader(int width, int height, int interlaceMethod) {
    return ByteBuffer.allocate(13)
        .putInt(width)
        .putInt(height)
        .put((byte) 8)
        .put((byte) RGBA_COLOR_TYPE)
        .put((byte) 0)
        .put((byte) 0)
        .put((byte) interlaceMethod)
        .array();
  }

  public static byte[] chunk(String chunkType, byte[] data) {
    byte[] typeBytes = chunkType.getBytes(StandardCharsets.US_ASCII);
    CRC32 crc = new CRC32();
    crc.update(typeBytes);
    crc.update(data);
    return ByteBuffer.allocate(12 + data.length)
        .putInt(data.length)
        .put(typeBytes)
        .put(data)
        .putInt((int) crc.getValue())
        .array();
  }

  public static byte[] compress(byte[] rawData) {
    return compress(rawData, Deflater.DEFAULT_COMPRESSION);
  }

  public static byte[] compress(byte[] rawData, int compressionLevel) {
    Deflater deflater = new Deflater(compressionLevel);
    deflater.setInput(rawData);
    deflater.finish();
    ByteArrayOutputStream compressedData = new ByteArrayOutputStream();
    byte[] buffer = new byte[8192];
    while (!deflater.finished()) {
      compressedData.write(buffer, 0, deflater.deflate(buffer));
    }
    deflater.end();
    return compressedData.toByteArray();
  }

  public static byte[] rawImageData(int width, int height) {
    return new byte[height * (1 + width * 4)];
  }

  public static byte[] png(byte[]... chunks) {
    ByteArrayOutputStream png = new ByteArrayOutputStream();
    png.writeBytes(PNG_SIGNATURE);
    for (byte[] chunk : chunks) {
      png.writeBytes(chunk);
    }
    return png.toByteArray();
  }

  public static byte[] skinWithImageData(int width, int height, byte[] imageData) {
    return png(
        chunk("IHDR", imageHeader(width, height, 0)),
        chunk("IDAT", imageData),
        chunk("IEND", new byte[0]));
  }

  public static byte[] uncompressedSkin(int width, int height) {
    return skinWithImageData(
        width, height, compress(rawImageData(width, height), Deflater.NO_COMPRESSION));
  }

  public static byte[] validSkin(int width, int height) {
    return skinWithImageData(width, height, compress(rawImageData(width, height)));
  }
}
