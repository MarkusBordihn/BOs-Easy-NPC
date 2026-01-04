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

package de.markusbordihn.easynpc.client.texture;

import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.util.Objects;
import java.util.UUID;

public record TextureModelKey(UUID uuid, SkinModel skinModel, String resourceName) {

  public TextureModelKey(UUID uuid, SkinModel skinModel) {
    this(uuid, skinModel, "");
  }

  public String getSubType() {
    return skinModel != null ? skinModel.name() : "";
  }

  public UUID getUUID() {
    return uuid;
  }

  public SkinModel getSkinModel() {
    return skinModel;
  }

  public String getResourceName() {
    return resourceName != null ? resourceName : "";
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof TextureModelKey other)) {
      return false;
    }
    return Objects.equals(uuid, other.uuid) && Objects.equals(getSubType(), other.getSubType());
  }

  @Override
  public int hashCode() {
    return Objects.hash(uuid, getSubType());
  }

  @Override
  public String toString() {
    return "TextureModelKey{"
        + "uuid="
        + uuid
        + ", skinModel="
        + skinModel
        + ", subType='"
        + getSubType()
        + "', resourceName='"
        + resourceName
        + "'}";
  }
}
