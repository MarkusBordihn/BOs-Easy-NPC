/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.data.model;

public record ItemAttachmentPoint(
    ModelPartType attachPart,
    float offsetX,
    float offsetY,
    float offsetZ,
    float rotX,
    float rotY,
    float rotZ,
    float scale) {

  public static final ItemAttachmentPoint NONE =
      new ItemAttachmentPoint(ModelPartType.UNKNOWN, 0, 0, 0, 0, 0, 0, 0);

  public static ItemAttachmentPoint mouth(ModelPartType headPart) {
    return new ItemAttachmentPoint(
        headPart, 0.0F, 1.0F, -8.0F, 0.0F, 0.0F, (float) (Math.PI / 2), 0.5F);
  }

  public static ItemAttachmentPoint wing(ModelPartType wingPart) {
    return new ItemAttachmentPoint(wingPart, 0.0F, 5.0F, -1.0F, 0.0F, 0.0F, 0.0F, 0.5F);
  }

  public static ItemAttachmentPoint arm(ModelPartType armPart) {
    return new ItemAttachmentPoint(
        armPart, -1.0F, 10.0F, -1.0F, (float) (-Math.PI / 2), 0.0F, 0.0F, 0.6F);
  }

  public static ItemAttachmentPoint body(ModelPartType bodyPart) {
    return new ItemAttachmentPoint(bodyPart, 5.0F, 10.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.5F);
  }

  public boolean isNone() {
    return this == NONE || attachPart == ModelPartType.UNKNOWN;
  }
}
