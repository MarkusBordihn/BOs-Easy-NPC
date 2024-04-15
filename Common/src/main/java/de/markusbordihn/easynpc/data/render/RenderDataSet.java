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
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RenderDataSet {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String DATA_RENDER_TYPE_TAG = "Type";
  private static final String DATA_RENDER_ENTITY_TYPE_TAG = "EntityType";

  private RenderType renderType = RenderType.DEFAULT;
  private EntityType<?> renderEntityType = null;

  public RenderDataSet() {}

  public RenderDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public RenderType getRenderType() {
    return this.renderType;
  }

  public void setRenderType(RenderType renderType) {
    this.renderType = renderType;
  }

  public EntityType<?> getRenderEntityType() {
    return this.renderEntityType;
  }

  public void setRenderEntityType(String entityTypeName) {
    EntityType<?> entityType = EntityType.byString(entityTypeName).orElse(null);
    if (entityType != null && !entityType.canSerialize()) {
      log.error(
          "Error Not serializable render entity type {} from {}!", entityType, entityTypeName);
    }
    this.renderEntityType = entityType;
    this.renderType = this.renderEntityType != null ? RenderType.CUSTOM_ENTITY : RenderType.DEFAULT;
  }

  public void setRenderEntityType(EntityType<?> renderEntityType) {
    this.renderEntityType = renderEntityType;
  }

  public void load(CompoundTag compoundTag) {
    this.renderType =
        compoundTag.contains(DATA_RENDER_TYPE_TAG)
            ? RenderType.get(compoundTag.getString(DATA_RENDER_TYPE_TAG))
            : RenderType.DEFAULT;

    this.renderEntityType =
        compoundTag.contains(DATA_RENDER_ENTITY_TYPE_TAG)
            ? EntityType.byString(compoundTag.getString(DATA_RENDER_ENTITY_TYPE_TAG)).orElse(null)
            : null;
  }

  public CompoundTag save(CompoundTag compoundTag) {
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
    return this.save(new CompoundTag());
  }
}
