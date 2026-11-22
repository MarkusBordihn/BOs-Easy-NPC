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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.zip.CRC32;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.InflaterInputStream;

public class PngCompressionUtils {

  public static final int MAX_COMPRESSION_LEVEL = Deflater.BEST_COMPRESSION;
  private static final int SIGNATURE_LENGTH = 8;
  private static final int CHUNK_OVERHEAD = 12;
  private static final byte[] IMAGE_DATA_CHUNK_TYPE = "IDAT".getBytes(StandardCharsets.US_ASCII);

  private PngCompressionUtils() {}

  public static byte[] recompress(byte[] sanitizedPngBytes, int compressionLevel) {
    if (compressionLevel <= 0 || compressionLevel > MAX_COMPRESSION_LEVEL) {
      return sanitizedPngBytes;
    }

    ByteArrayOutputStream leadingChunks = new ByteArrayOutputStream();
    leadingChunks.write(sanitizedPngBytes, 0, SIGNATURE_LENGTH);
    ByteArrayOutputStream imageData = new ByteArrayOutputStream();
    ByteArrayOutputStream trailingChunks = new ByteArrayOutputStream();
    int position = SIGNATURE_LENGTH;
    while (position < sanitizedPngBytes.length) {
      int chunkLength = ByteBuffer.wrap(sanitizedPngBytes, position, 4).getInt();
      int chunkEnd = position + chunkLength + CHUNK_OVERHEAD;
      if (Arrays.equals(
          sanitizedPngBytes, position + 4, position + 8, IMAGE_DATA_CHUNK_TYPE, 0, 4)) {
        imageData.write(sanitizedPngBytes, position + 8, chunkLength);
      } else if (imageData.size() == 0) {
        leadingChunks.write(sanitizedPngBytes, position, chunkEnd - position);
      } else {
        trailingChunks.write(sanitizedPngBytes, position, chunkEnd - position);
      }
      position = chunkEnd;
    }

    byte[] recompressedImageData;
    try {
      recompressedImageData = recompressImageData(imageData.toByteArray(), compressionLevel);
    } catch (IOException e) {
      return sanitizedPngBytes;
    }
    if (leadingChunks.size() + recompressedImageData.length + CHUNK_OVERHEAD + trailingChunks.size()
        >= sanitizedPngBytes.length) {
      return sanitizedPngBytes;
    }

    ByteArrayOutputStream recompressedPng = new ByteArrayOutputStream(sanitizedPngBytes.length);
    recompressedPng.writeBytes(leadingChunks.toByteArray());
    writeImageDataChunk(recompressedPng, recompressedImageData);
    recompressedPng.writeBytes(trailingChunks.toByteArray());
    return recompressedPng.toByteArray();
  }

  private static byte[] recompressImageData(byte[] imageData, int compressionLevel)
      throws IOException {
    ByteArrayOutputStream recompressedImageData = new ByteArrayOutputStream(imageData.length);
    Deflater deflater = new Deflater(compressionLevel);
    try (InflaterInputStream inflaterInputStream =
            new InflaterInputStream(new ByteArrayInputStream(imageData));
        DeflaterOutputStream deflaterOutputStream =
            new DeflaterOutputStream(recompressedImageData, deflater)) {
      inflaterInputStream.transferTo(deflaterOutputStream);
    } finally {
      deflater.end();
    }
    return recompressedImageData.toByteArray();
  }

  private static void writeImageDataChunk(ByteArrayOutputStream pngBytes, byte[] imageData) {
    CRC32 crc = new CRC32();
    crc.update(IMAGE_DATA_CHUNK_TYPE);
    crc.update(imageData);
    pngBytes.writeBytes(ByteBuffer.allocate(4).putInt(imageData.length).array());
    pngBytes.writeBytes(IMAGE_DATA_CHUNK_TYPE);
    pngBytes.writeBytes(imageData);
    pngBytes.writeBytes(ByteBuffer.allocate(4).putInt((int) crc.getValue()).array());
  }
}
