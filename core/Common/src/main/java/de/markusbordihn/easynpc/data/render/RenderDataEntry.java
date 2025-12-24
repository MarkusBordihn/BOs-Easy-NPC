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

package de.markusbordihn.easynpc.data.render;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;

public record RenderDataEntry(
    RenderType renderType, EntityType<? extends Entity> renderEntityType) {

  static final String DATA_RENDER_TYPE_TAG = "Type";
  static final String DATA_RENDER_ENTITY_TYPE_TAG = "EntityType";
  public static final StreamCodec<RegistryFriendlyByteBuf, RenderDataEntry> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public RenderDataEntry decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new RenderDataEntry(registryFriendlyByteBuf.readNbt());
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf, RenderDataEntry renderDataEntry) {
          registryFriendlyByteBuf.writeNbt(renderDataEntry.createTag());
        }
      };

  public RenderDataEntry() {
    this(RenderType.DEFAULT, null);
  }

  public RenderDataEntry(final CompoundTag compoundTag) {
    this(
        compoundTag.contains(DATA_RENDER_TYPE_TAG)
            ? RenderType.get(compoundTag.getString(DATA_RENDER_TYPE_TAG))
            : RenderType.DEFAULT,
        compoundTag.contains(DATA_RENDER_ENTITY_TYPE_TAG)
            ? EntityType.byString(compoundTag.getString(DATA_RENDER_ENTITY_TYPE_TAG)).orElse(null)
            : null);
  }

  public RenderDataEntry withRenderType(final RenderType renderType) {
    return new RenderDataEntry(
        renderType, renderType == RenderType.DEFAULT ? null : renderEntityType);
  }

  public RenderDataEntry withRenderEntityType(final EntityType<? extends Entity> renderEntityType) {
    return new RenderDataEntry(
        renderEntityType != null ? RenderType.CUSTOM_ENTITY : RenderType.DEFAULT, renderEntityType);
  }

  public RenderDataEntry create(CompoundTag compoundTag) {
    return new RenderDataEntry(compoundTag);
  }

  public CompoundTag write(CompoundTag compoundTag) {
    if (this.renderType != RenderType.DEFAULT) {
      compoundTag.putString(DATA_RENDER_TYPE_TAG, this.renderType.name());
    }

    if (this.renderEntityType != null && this.renderEntityType.canSerialize()) {
      ResourceLocation entityTypeResourceLocation = EntityType.getKey(this.renderEntityType);
      compoundTag.putString(DATA_RENDER_ENTITY_TYPE_TAG, entityTypeResourceLocation.toString());
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return write(new CompoundTag());
  }

  public CompoundTag save(CompoundTag compoundTag) {
    return write(compoundTag);
  }

  public RenderType getRenderType() {
    return renderType;
  }

  public EntityType<? extends Entity> getRenderEntityType() {
    return renderEntityType;
  }
}
