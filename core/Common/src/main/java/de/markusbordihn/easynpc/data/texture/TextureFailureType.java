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

package de.markusbordihn.easynpc.data.texture;

public enum TextureFailureType {
  INVALID_IMAGE_SIZE(true, "Image dimensions invalid"),
  DECODING_ERROR(true, "Failed to decode image"),
  INVALID_FORMAT(true, "Unsupported image format"),
  FILE_TOO_LARGE(true, "File exceeds size limit"),
  NETWORK_ERROR(false, "Network connection failed"),
  URL_INVALID(true, "URL format invalid"),
  TIMEOUT(false, "Connection timeout"),
  MAX_RETRIES_EXCEEDED(true, "Maximum retry attempts exceeded");

  private final boolean permanent;
  private final String message;

  TextureFailureType(boolean permanent, String message) {
    this.permanent = permanent;
    this.message = message;
  }

  public boolean isPermanent() {
    return permanent;
  }

  public String getMessage() {
    return message;
  }
}
