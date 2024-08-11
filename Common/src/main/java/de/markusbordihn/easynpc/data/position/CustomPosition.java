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

package de.markusbordihn.easynpc.data.position;

import net.minecraft.nbt.FloatTag;
import net.minecraft.nbt.ListTag;

public record CustomPosition(float x, float y, float z) {

  public CustomPosition(ListTag listTag) {
    this(listTag.getFloat(0), listTag.getFloat(1), listTag.getFloat(2));
  }

  public ListTag save() {
    ListTag listTag = new ListTag();
    listTag.add(FloatTag.valueOf(this.x));
    listTag.add(FloatTag.valueOf(this.y));
    listTag.add(FloatTag.valueOf(this.z));
    return listTag;
  }

  public boolean hasChanged() {
    return hasChanged(0, 0, 0);
  }

  public boolean hasChanged(float x, float y, float z) {
    return this.x != x || this.y != y || this.z != z;
  }
}
