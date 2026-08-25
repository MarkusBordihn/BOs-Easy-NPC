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

import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Pose;

public record EntityRenderConfig(
    int x,
    int y,
    int scale,
    float rotationYaw,
    float rotationPitch,
    EntityRenderOverrides overrides) {

  public static EntityRenderConfig guiScaled(
      int x, int y, int scale, float rotationYaw, float rotationPitch) {
    return new EntityRenderConfig(
        x, y, scale, rotationYaw, rotationPitch, EntityRenderOverrides.HIDE_NAME_TAG_RESET_ROOT);
  }

  public static EntityRenderConfig dialog(
      int x, int y, int scale, float rotationYaw, float rotationPitch) {
    return new EntityRenderConfig(
        x, y, scale, rotationYaw, rotationPitch, EntityRenderOverrides.HIDE_NAME_TAG);
  }

  public static EntityRenderConfig customPose(
      int x, int y, int scale, float rotationYaw, float rotationPitch) {
    return new EntityRenderConfig(
        x,
        y,
        scale,
        rotationYaw,
        rotationPitch,
        EntityRenderOverrides.withCustomPose(ModelPose.CUSTOM, Pose.STANDING)
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

  public static EntityRenderConfig customModel(
      int x, int y, int scale, float rotationYaw, float rotationPitch, EntityType<?> entityType) {
    return new EntityRenderConfig(
        x,
        y,
        scale,
        rotationYaw,
        rotationPitch,
        EntityRenderOverrides.withCustomModel(RenderType.CUSTOM, entityType)
            .withHideNameTag(true));
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
