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

package de.markusbordihn.easynpc.data.custom;

import net.minecraft.network.syncher.EntityDataSerializer;

public class CustomDataAccessor<T> {

  private final CustomDataIndex customDataIndex;
  private final EntityDataSerializer<T> entityDataSerializer;

  public CustomDataAccessor(int index, EntityDataSerializer<T> entityDataSerializer) {
    this(CustomDataIndex.getIndex(index), entityDataSerializer);
  }

  public CustomDataAccessor(
      CustomDataIndex customDataIndex, EntityDataSerializer<T> entityDataSerializer) {
    this.customDataIndex = customDataIndex;
    this.entityDataSerializer = entityDataSerializer;
  }

  public CustomDataIndex getIndex() {
    return this.customDataIndex;
  }

  public EntityDataSerializer<T> getSerializer() {
    return this.entityDataSerializer;
  }

  public boolean equals(Object object) {
    if (this == object) {
      return true;
    } else if (object != null && this.getClass() == object.getClass()) {
      CustomDataAccessor<?> entityDataAccessor = (CustomDataAccessor<?>) object;
      return this.customDataIndex == entityDataAccessor.customDataIndex;
    } else {
      return false;
    }
  }

  public int hashCode() {
    return this.customDataIndex.hashCode();
  }

  public String toString() {
    return "<custom data: "
        + this.customDataIndex
        + " serializer:"
        + this.entityDataSerializer
        + ">";
  }
}
