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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.zip.CRC32;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

public class PngFileValidator {

  public static final int MAX_FILE_SIZE = 4 * 1024 * 1024;
  public static final int MAX_DIMENSION = 2048;
  private static final int MAX_ANCILLARY_CHUNK_COUNT = 64;
  private static final int CHUNK_OVERHEAD = 12;
  private static final int IMAGE_HEADER_LENGTH = 13;
  private static final byte[] PNG_SIGNATURE = {
    (byte) 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A
  };
  private static final Set<String> KEPT_ANCILLARY_CHUNK_TYPES =
      Set.of("tRNS", "gAMA", "cHRM", "sRGB", "sBIT", "bKGD", "pHYs", "tIME");
  private static final int[][] ADAM7_PASSES = {
    {0, 0, 8, 8}, {4, 0, 8, 8}, {0, 4, 4, 8}, {2, 0, 4, 4}, {0, 2, 2, 4}, {1, 0, 2, 2}, {0, 1, 1, 2}
  };

  private PngFileValidator() {}

  public static boolean isValidSkinSize(int width, int height) {
    if (width == 48 && height == 32) {
      return true;
    }

    return width >= 32
        && height >= 32
        && width <= MAX_DIMENSION
        && height <= MAX_DIMENSION
        && width % 32 == 0
        && height % 32 == 0;
  }

  public static Result sanitize(byte[] pngBytes) {
    if (pngBytes == null
        || pngBytes.length < PNG_SIGNATURE.length
        || !Arrays.equals(
            pngBytes, 0, PNG_SIGNATURE.length, PNG_SIGNATURE, 0, PNG_SIGNATURE.length)) {
      return Result.rejected("missing PNG signature");
    }
    if (pngBytes.length > MAX_FILE_SIZE) {
      return Result.rejected("file is larger than " + MAX_FILE_SIZE + " bytes");
    }

    ByteArrayOutputStream sanitizedPng = new ByteArrayOutputStream(pngBytes.length);
    sanitizedPng.write(PNG_SIGNATURE, 0, PNG_SIGNATURE.length);
    ByteArrayOutputStream imageData = new ByteArrayOutputStream();
    List<String> removedChunkTypes = new ArrayList<>();
    ImageHeader imageHeader = null;
    boolean hasPalette = false;
    boolean hasImageData = false;
    boolean imageDataClosed = false;
    boolean hasEnd = false;
    int ancillaryChunkCount = 0;
    int position = PNG_SIGNATURE.length;

    while (position < pngBytes.length) {
      if (hasEnd) {
        return Result.rejected("data after IEND chunk");
      }
      if (pngBytes.length - position < CHUNK_OVERHEAD) {
        return Result.rejected("truncated chunk");
      }

      long chunkLength = readUnsignedInt(pngBytes, position);
      if (chunkLength > pngBytes.length - position - CHUNK_OVERHEAD) {
        return Result.rejected("chunk length exceeds file size");
      }
      String chunkType = readChunkType(pngBytes, position + 4);
      if (chunkType == null) {
        return Result.rejected("invalid chunk type");
      }

      int dataStart = position + 8;
      int dataLength = (int) chunkLength;
      int chunkEnd = dataStart + dataLength + 4;
      CRC32 crc = new CRC32();
      crc.update(pngBytes, position + 4, 4 + dataLength);
      if (crc.getValue() != readUnsignedInt(pngBytes, dataStart + dataLength)) {
        return Result.rejected("CRC mismatch in " + chunkType + " chunk");
      }
      if (imageHeader == null && !"IHDR".equals(chunkType)) {
        return Result.rejected("IHDR is not the first chunk");
      }
      if (hasImageData && !"IDAT".equals(chunkType)) {
        imageDataClosed = true;
      }

      switch (chunkType) {
        case "IHDR" -> {
          if (imageHeader != null) {
            return Result.rejected("duplicate IHDR chunk");
          }
          imageHeader = ImageHeader.read(pngBytes, dataStart, dataLength);
          if (imageHeader == null) {
            return Result.rejected("invalid IHDR chunk");
          }
          if (!isValidSkinSize(imageHeader.width(), imageHeader.height())) {
            return Result.rejected(
                "unsupported image size " + imageHeader.width() + "x" + imageHeader.height());
          }
        }
        case "PLTE" -> {
          if (hasPalette || hasImageData) {
            return Result.rejected("unexpected PLTE chunk");
          }
          hasPalette = true;
        }
        case "IDAT" -> {
          if (imageDataClosed) {
            return Result.rejected("IDAT chunks are not consecutive");
          }
          hasImageData = true;
          imageData.write(pngBytes, dataStart, dataLength);
        }
        case "IEND" -> {
          if (dataLength != 0) {
            return Result.rejected("IEND chunk with data");
          }
          hasEnd = true;
        }
        default -> {
          if (Character.isUpperCase(chunkType.charAt(0))) {
            return Result.rejected("unsupported critical chunk " + chunkType);
          }
          if (++ancillaryChunkCount > MAX_ANCILLARY_CHUNK_COUNT) {
            return Result.rejected("too many ancillary chunks");
          }
          if (!KEPT_ANCILLARY_CHUNK_TYPES.contains(chunkType)) {
            removedChunkTypes.add(chunkType);
            position = chunkEnd;
            continue;
          }
        }
      }

      sanitizedPng.write(pngBytes, position, chunkEnd - position);
      position = chunkEnd;
    }

    if (!hasEnd) {
      return Result.rejected("missing IEND chunk");
    }
    if (!hasImageData) {
      return Result.rejected("missing IDAT chunk");
    }
    if (imageHeader.colorType() == 3 && !hasPalette) {
      return Result.rejected("missing PLTE chunk for indexed image");
    }

    String imageDataError = checkImageData(imageData.toByteArray(), imageHeader);
    if (imageDataError != null) {
      return Result.rejected(imageDataError);
    }

    // Ears and similar skin mods store data in transparent pixels, so pixels are never re-encoded.
    return new Result(
        removedChunkTypes.isEmpty() ? pngBytes : sanitizedPng.toByteArray(),
        List.copyOf(removedChunkTypes),
        null);
  }

  private static String checkImageData(byte[] compressedImageData, ImageHeader imageHeader) {
    long expectedSize = imageHeader.getRawImageDataSize();
    Inflater inflater = new Inflater();
    try {
      inflater.setInput(compressedImageData);
      byte[] buffer = new byte[8192];
      long inflatedSize = 0;
      while (!inflater.finished()) {
        int inflatedBytes = inflater.inflate(buffer);
        if (inflatedBytes == 0) {
          return "image data stream is truncated";
        }
        inflatedSize += inflatedBytes;
        if (inflatedSize > expectedSize) {
          return "image data is larger than the image size";
        }
      }
      if (inflatedSize != expectedSize) {
        return "image data is smaller than the image size";
      }
      if (inflater.getRemaining() > 0) {
        return "data after the image data stream";
      }
      return null;
    } catch (DataFormatException e) {
      return "invalid image data stream";
    } finally {
      inflater.end();
    }
  }

  private static long readUnsignedInt(byte[] bytes, int offset) {
    return Integer.toUnsignedLong(ByteBuffer.wrap(bytes, offset, 4).getInt());
  }

  private static String readChunkType(byte[] bytes, int offset) {
    char[] chunkType = new char[4];
    for (int i = 0; i < 4; i++) {
      char character = (char) (bytes[offset + i] & 0xFF);
      if (!(character >= 'A' && character <= 'Z') && !(character >= 'a' && character <= 'z')) {
        return null;
      }
      chunkType[i] = character;
    }
    return new String(chunkType);
  }

  public record Result(byte[] pngBytes, List<String> removedChunkTypes, String rejectionReason) {

    private static Result rejected(String rejectionReason) {
      return new Result(null, List.of(), rejectionReason);
    }

    public boolean isValid() {
      return this.rejectionReason == null;
    }
  }

  private record ImageHeader(
      int width, int height, int bitDepth, int colorType, int interlaceMethod) {

    private static ImageHeader read(byte[] bytes, int offset, int length) {
      if (length != IMAGE_HEADER_LENGTH) {
        return null;
      }

      long width = readUnsignedInt(bytes, offset);
      long height = readUnsignedInt(bytes, offset + 4);
      int bitDepth = bytes[offset + 8] & 0xFF;
      int colorType = bytes[offset + 9] & 0xFF;
      int compressionMethod = bytes[offset + 10] & 0xFF;
      int filterMethod = bytes[offset + 11] & 0xFF;
      int interlaceMethod = bytes[offset + 12] & 0xFF;
      if (width > MAX_DIMENSION
          || height > MAX_DIMENSION
          || compressionMethod != 0
          || filterMethod != 0
          || interlaceMethod > 1
          || !isValidBitDepth(colorType, bitDepth)) {
        return null;
      }

      return new ImageHeader((int) width, (int) height, bitDepth, colorType, interlaceMethod);
    }

    private static boolean isValidBitDepth(int colorType, int bitDepth) {
      return switch (colorType) {
        case 0 ->
            bitDepth == 1 || bitDepth == 2 || bitDepth == 4 || bitDepth == 8 || bitDepth == 16;
        case 3 -> bitDepth == 1 || bitDepth == 2 || bitDepth == 4 || bitDepth == 8;
        case 2, 4, 6 -> bitDepth == 8 || bitDepth == 16;
        default -> false;
      };
    }

    private static long getScanlinesSize(long width, long height, int bitsPerPixel) {
      return height * (1 + (width * bitsPerPixel + 7) / 8);
    }

    private long getRawImageDataSize() {
      int channels =
          switch (this.colorType) {
            case 2 -> 3;
            case 4 -> 2;
            case 6 -> 4;
            default -> 1;
          };
      int bitsPerPixel = channels * this.bitDepth;
      if (this.interlaceMethod == 0) {
        return getScanlinesSize(this.width, this.height, bitsPerPixel);
      }

      long rawImageDataSize = 0;
      for (int[] pass : ADAM7_PASSES) {
        long passWidth = (this.width - pass[0] + pass[2] - 1L) / pass[2];
        long passHeight = (this.height - pass[1] + pass[3] - 1L) / pass[3];
        if (passWidth > 0 && passHeight > 0) {
          rawImageDataSize += getScanlinesSize(passWidth, passHeight, bitsPerPixel);
        }
      }
      return rawImageDataSize;
    }
  }
}
