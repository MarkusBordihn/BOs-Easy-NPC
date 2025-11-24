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

package de.markusbordihn.easynpc.data.rotation;

import de.markusbordihn.easynpc.data.model.ModelPartType;
import java.util.List;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.FloatTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;

public record CustomRotation(float x, float y, float z, boolean locked) {

  public static final CustomRotation DEFAULT = new CustomRotation(0, 0, 0, false);

  public static final StreamCodec<RegistryFriendlyByteBuf, CustomRotation> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public CustomRotation decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new CustomRotation(
              registryFriendlyByteBuf.readFloat(),
              registryFriendlyByteBuf.readFloat(),
              registryFriendlyByteBuf.readFloat(),
              false);
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf, CustomRotation rotation) {
          registryFriendlyByteBuf.writeFloat(rotation.x);
          registryFriendlyByteBuf.writeFloat(rotation.y);
          registryFriendlyByteBuf.writeFloat(rotation.z);
          registryFriendlyByteBuf.writeBoolean(rotation.locked);
        }
      };

  public CustomRotation(float x, float y, float z) {
    this(x, y, z, false);
  }

  public CustomRotation(ModelPartType modelPartType, CompoundTag compoundTag) {
    this(compoundTag.getList(modelPartType.getTagName(), 5));
  }

  public CustomRotation(ListTag listTag) {
    this(
        listTag != null && listTag.size() > 0 ? listTag.getFloat(0) : 0f,
        listTag != null && listTag.size() > 1 ? listTag.getFloat(1) : 0f,
        listTag != null && listTag.size() > 2 ? listTag.getFloat(2) : 0f,
        listTag != null && listTag.size() > 3 && listTag.getFloat(3) == 1.0F);
  }

  public CustomRotation(List<Float> list) {
    this(
        list != null && list.size() > 0 ? list.get(0) : 0f,
        list != null && list.size() > 1 ? list.get(1) : 0f,
        list != null && list.size() > 2 ? list.get(2) : 0f,
        list != null && list.size() > 3 && list.get(3) == 1.0F);
  }

  public CustomRotation withLocked(boolean locked) {
    return new CustomRotation(this.x, this.y, this.z, locked);
  }

  public ListTag save() {
    ListTag listTag = new ListTag();
    listTag.add(FloatTag.valueOf(this.x));
    listTag.add(FloatTag.valueOf(this.y));
    listTag.add(FloatTag.valueOf(this.z));
    listTag.add(FloatTag.valueOf(this.locked ? 1.0F : 0.0F));
    return listTag;
  }

  public boolean hasChanged() {
    return hasChanged(0, 0, 0);
  }

  public boolean hasChangedYaw() {
    return this.x == 0 && this.y != 0 && this.z == 0 && this.locked;
  }

  public boolean hasChanged(float x, float y, float z) {
    return hasChanged(x, y, z, false);
  }

  public boolean hasChanged(float x, float y, float z, boolean locked) {
    return this.x != x || this.y != y || this.z != z || this.locked != locked;
  }
}
