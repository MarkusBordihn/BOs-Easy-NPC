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

import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import net.minecraft.world.entity.EntityType;

public record EntityRenderConfig(
    int x,
    int y,
    int scale,
    float rotationYaw,
    float rotationPitch,
    int left,
    int top,
    int right,
    int bottom,
    float yOffset,
    EntityRenderOverrides overrides) {

  private static final int DEFAULT_RENDER_WIDTH = 300;
  private static final int DEFAULT_RENDER_HEIGHT = 300;

  private EntityRenderConfig(
      int x,
      int y,
      int scale,
      float rotationYaw,
      float rotationPitch,
      EntityRenderOverrides overrides) {
    this(
        x,
        y,
        scale,
        rotationYaw,
        rotationPitch,
        x - DEFAULT_RENDER_WIDTH / 2,
        y - DEFAULT_RENDER_HEIGHT / 2,
        x + DEFAULT_RENDER_WIDTH / 2,
        y + DEFAULT_RENDER_HEIGHT / 2,
        0.0f,
        overrides);
  }

  private EntityRenderConfig(
      int x,
      int y,
      int scale,
      float rotationYaw,
      float rotationPitch,
      int width,
      int height,
      EntityRenderOverrides overrides) {
    this(
        x,
        y,
        scale,
        rotationYaw,
        rotationPitch,
        x - width / 2,
        y - height / 2,
        x + width / 2,
        y + height / 2,
        0.0f,
        overrides);
  }

  private EntityRenderConfig(
      int x,
      int y,
      int scale,
      float rotationYaw,
      float rotationPitch,
      float yOffset,
      EntityRenderOverrides overrides) {
    this(
        x,
        y,
        scale,
        rotationYaw,
        rotationPitch,
        x - DEFAULT_RENDER_WIDTH / 2,
        y - DEFAULT_RENDER_HEIGHT / 2,
        x + DEFAULT_RENDER_WIDTH / 2,
        y + DEFAULT_RENDER_HEIGHT / 2,
        yOffset,
        overrides);
  }

  public static EntityRenderConfig guiScaled(int x, int y, int scale) {
    return guiScaled(x, y, scale, 0, 0);
  }

  public static EntityRenderConfig guiScaled(
      int x, int y, int scale, float rotationYaw, float rotationPitch) {
    return new EntityRenderConfig(
        x, y, scale, rotationYaw, rotationPitch, EntityRenderOverrides.HIDE_NAME_TAG_RESET_ROOT);
  }

  public static EntityRenderConfig dialog(int x, int y, int scale) {
    return dialog(x, y, scale, 0, 0);
  }

  public static EntityRenderConfig dialog(
      int x, int y, int scale, float rotationYaw, float rotationPitch) {
    return new EntityRenderConfig(
        x, y, scale, rotationYaw, rotationPitch, EntityRenderOverrides.HIDE_NAME_TAG_RESET_ROOT);
  }

  public static EntityRenderConfig scaling(int x, int y, int scale) {
    return scaling(x, y, scale, 0.0f);
  }

  public static EntityRenderConfig scaling(int x, int y, int scale, float yOffset) {
    return new EntityRenderConfig(
        x,
        y,
        scale,
        0,
        0,
        yOffset,
        EntityRenderOverrides.NONE
            .withRootRotation(new CustomRotation(0.0F, 0.0F, 0.0F))
            .withHideNameTag(true));
  }

  public static EntityRenderConfig scaling(
      int x, int y, int scale, float rotationYaw, float rotationPitch) {
    return new EntityRenderConfig(
        x,
        y,
        scale,
        rotationYaw,
        rotationPitch,
        EntityRenderOverrides.NONE
            .withRootRotation(new CustomRotation(0.0F, 0.0F, 0.0F))
            .withHideNameTag(true));
  }

  public static EntityRenderConfig customModel(int x, int y, int scale, EntityType<?> entityType) {
    return customModel(x, y, scale, 0, 0, entityType);
  }

  public static EntityRenderConfig customModel(
      int x, int y, int scale, float rotationYaw, float rotationPitch, EntityType<?> entityType) {
    return new EntityRenderConfig(
        x,
        y,
        scale,
        rotationYaw,
        rotationPitch,
        EntityRenderOverrides.withCustomModel(RenderType.CUSTOM, entityType));
  }

  public static EntityRenderConfig withOverrides(
      int x, int y, int scale, EntityRenderOverrides overrides) {
    return withOverrides(x, y, scale, 0, 0, overrides);
  }

  public static EntityRenderConfig withOverrides(
      int x,
      int y,
      int scale,
      float rotationYaw,
      float rotationPitch,
      EntityRenderOverrides overrides) {
    return new EntityRenderConfig(x, y, scale, rotationYaw, rotationPitch, overrides);
  }
}
