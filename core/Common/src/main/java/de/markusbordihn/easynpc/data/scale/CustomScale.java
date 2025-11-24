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

package de.markusbordihn.easynpc.data.scale;

import de.markusbordihn.easynpc.data.model.ModelPartType;
import java.util.List;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.FloatTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;

public record CustomScale(float x, float y, float z) {

  public static final CustomScale DEFAULT = new CustomScale(1.0f, 1.0f, 1.0f);

  public static final StreamCodec<RegistryFriendlyByteBuf, CustomScale> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public CustomScale decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new CustomScale(
              registryFriendlyByteBuf.readFloat(),
              registryFriendlyByteBuf.readFloat(),
              registryFriendlyByteBuf.readFloat());
        }

        @Override
        public void encode(RegistryFriendlyByteBuf registryFriendlyByteBuf, CustomScale scale) {
          registryFriendlyByteBuf.writeFloat(scale.x);
          registryFriendlyByteBuf.writeFloat(scale.y);
          registryFriendlyByteBuf.writeFloat(scale.z);
        }
      };

  public CustomScale(ModelPartType modelPartType, CompoundTag compoundTag) {
    this(compoundTag.getList(modelPartType.getTagName(), 5));
  }

  public CustomScale(ListTag listTag) {
    this(
        listTag != null && listTag.size() > 0 ? listTag.getFloat(0) : 1.0f,
        listTag != null && listTag.size() > 1 ? listTag.getFloat(1) : 1.0f,
        listTag != null && listTag.size() > 2 ? listTag.getFloat(2) : 1.0f);
  }

  public CustomScale(List<Float> list) {
    this(
        list != null && list.size() > 0 ? list.get(0) : 1.0f,
        list != null && list.size() > 1 ? list.get(1) : 1.0f,
        list != null && list.size() > 2 ? list.get(2) : 1.0f);
  }

  public CustomScale(float scale) {
    this(scale, scale, scale);
  }

  public ListTag save() {
    ListTag listTag = new ListTag();
    listTag.add(FloatTag.valueOf(this.x));
    listTag.add(FloatTag.valueOf(this.y));
    listTag.add(FloatTag.valueOf(this.z));
    return listTag;
  }

  public boolean hasChanged() {
    return hasChanged(1, 1, 1);
  }

  public boolean hasChanged(float x, float y, float z) {
    return this.x != x || this.y != y || this.z != z;
  }
}
