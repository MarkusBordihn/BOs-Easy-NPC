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

import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;

public record RootModelData(CustomRotation rotation, CustomScale scale) {

  public static final RootModelData DEFAULT =
      new RootModelData(CustomRotation.DEFAULT, CustomScale.DEFAULT);

  public static final StreamCodec<RegistryFriendlyByteBuf, RootModelData> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public RootModelData decode(RegistryFriendlyByteBuf buf) {
          return new RootModelData(
              CustomRotation.STREAM_CODEC.decode(buf), CustomScale.STREAM_CODEC.decode(buf));
        }

        @Override
        public void encode(RegistryFriendlyByteBuf buf, RootModelData data) {
          CustomRotation.STREAM_CODEC.encode(buf, data.rotation);
          CustomScale.STREAM_CODEC.encode(buf, data.scale);
        }
      };

  private static final String ROTATION_TAG = "Rotation";
  private static final String SCALE_TAG = "Scale";

  public static RootModelData load(CompoundTag compoundTag) {
    return new RootModelData(
        compoundTag.contains(ROTATION_TAG)
            ? new CustomRotation(compoundTag.getList(ROTATION_TAG, 5))
            : CustomRotation.DEFAULT,
        compoundTag.contains(SCALE_TAG)
            ? new CustomScale(compoundTag.getList(SCALE_TAG, 5))
            : CustomScale.DEFAULT);
  }

  public boolean isRotationLocked() {
    return rotation.locked();
  }

  public boolean hasChanged() {
    return rotation.hasChanged() || scale.hasChanged();
  }

  public CompoundTag save() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put(ROTATION_TAG, rotation.save());
    if (scale.hasChanged()) {
      compoundTag.put(SCALE_TAG, scale.save());
    }

    return compoundTag;
  }
}
