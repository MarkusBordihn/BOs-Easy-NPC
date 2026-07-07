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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.model.ModelType;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record RenderDataEntry(
    RenderType renderType,
    EntityType<? extends Entity> renderEntityType,
    String renderEntityModel,
    ModelType renderModelType) {

  static final String DATA_RENDER_TYPE_TAG = "Type";
  static final String DATA_RENDER_ENTITY_TYPE_TAG = "EntityType";
  static final String DATA_RENDER_ENTITY_MODEL_TAG = "EntityModel";
  static final String DATA_RENDER_MODEL_TYPE_TAG = "ModelType";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public RenderDataEntry() {
    this(RenderType.DEFAULT, null, null, null);
  }

  public RenderDataEntry(
      final RenderType renderType, final EntityType<? extends Entity> renderEntityType) {
    this(renderType, renderEntityType, null, null);
  }

  public RenderDataEntry(
      final RenderType renderType,
      final EntityType<? extends Entity> renderEntityType,
      final String renderEntityModel) {
    this(renderType, renderEntityType, renderEntityModel, null);
  }

  public RenderDataEntry(final CompoundTag compoundTag) {
    this(
        compoundTag.contains(DATA_RENDER_TYPE_TAG)
            ? RenderType.get(compoundTag.getString(DATA_RENDER_TYPE_TAG))
            : RenderType.DEFAULT,
        compoundTag.contains(DATA_RENDER_ENTITY_TYPE_TAG)
            ? EntityType.byString(compoundTag.getString(DATA_RENDER_ENTITY_TYPE_TAG)).orElse(null)
            : null,
        compoundTag.contains(DATA_RENDER_ENTITY_MODEL_TAG)
            ? compoundTag.getString(DATA_RENDER_ENTITY_MODEL_TAG)
            : null,
        parseModelType(compoundTag));
  }

  private static ModelType parseModelType(final CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_RENDER_MODEL_TYPE_TAG)) {
      return null;
    }
    try {
      return ModelType.valueOf(compoundTag.getString(DATA_RENDER_MODEL_TYPE_TAG));
    } catch (IllegalArgumentException exception) {
      return null;
    }
  }

  public RenderDataEntry withRenderType(final RenderType renderType) {
    return new RenderDataEntry(
        renderType,
        renderType == RenderType.DEFAULT ? null : renderEntityType,
        renderEntityModel,
        renderModelType);
  }

  public RenderDataEntry withRenderEntityType(final EntityType<? extends Entity> renderEntityType) {
    return new RenderDataEntry(
        renderEntityType != null ? RenderType.CUSTOM_ENTITY : RenderType.DEFAULT,
        renderEntityType,
        null,
        null);
  }

  public RenderDataEntry withRenderEntityModel(final String renderEntityModel) {
    if (renderEntityModel == null) {
      return new RenderDataEntry(RenderType.DEFAULT, null, null, null);
    }

    return new RenderDataEntry(this.renderType, null, renderEntityModel, null);
  }

  public RenderDataEntry withRenderModelType(final ModelType renderModelType) {
    return new RenderDataEntry(renderType, renderEntityType, renderEntityModel, renderModelType);
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

    if (this.renderEntityModel != null && !this.renderEntityModel.isEmpty()) {
      compoundTag.putString(DATA_RENDER_ENTITY_MODEL_TAG, this.renderEntityModel);
    }

    if (this.renderModelType != null) {
      compoundTag.putString(DATA_RENDER_MODEL_TYPE_TAG, this.renderModelType.name());
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

  public String getRenderEntityModel() {
    return renderEntityModel;
  }

  public ModelType getRenderModelType() {
    return renderModelType;
  }
}
