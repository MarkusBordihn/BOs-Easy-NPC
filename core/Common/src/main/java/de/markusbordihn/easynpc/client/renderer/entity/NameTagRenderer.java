/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.client.renderer.entity;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.config.ClientNameTagConfig;
import de.markusbordihn.easynpc.config.OcclusionMode;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import org.joml.Matrix4f;

public class NameTagRenderer {

  private static final float NAME_TAG_SCALE = 0.025f;
  private static final float BACKGROUND_OPACITY = 0.25f;
  private static final int DIMMED_TEXT_COLOR = 553648127;
  private static final int VISIBLE_TEXT_COLOR = -1;

  private NameTagRenderer() {}

  public static void render(
      final Entity entity,
      final Component displayName,
      final PoseStack poseStack,
      final MultiBufferSource bufferSource,
      final int packedLight) {
    Minecraft minecraft = Minecraft.getInstance();
    EntityRenderDispatcher entityRenderDispatcher = minecraft.getEntityRenderDispatcher();
    double maxRenderDistance = ClientNameTagConfig.MAX_RENDER_DISTANCE_BLOCKS;
    if (entityRenderDispatcher.distanceToSqr(entity) > maxRenderDistance * maxRenderDistance) {
      return;
    }

    OcclusionMode occlusionMode =
        entity.isDiscrete() ? OcclusionMode.NEVER : ClientNameTagConfig.OCCLUSION_MODE;

    poseStack.pushPose();
    poseStack.translate(0.0f, entity.getNameTagOffsetY(), 0.0f);
    poseStack.mulPose(entityRenderDispatcher.cameraOrientation());
    poseStack.scale(-NAME_TAG_SCALE, -NAME_TAG_SCALE, NAME_TAG_SCALE);

    Font font = minecraft.font;
    Matrix4f pose = poseStack.last().pose();
    float textOffsetX = -font.width(displayName) / 2.0f;
    int backgroundColor =
        (int) (minecraft.options.getBackgroundOpacity(BACKGROUND_OPACITY) * 255.0f) << 24;

    if (occlusionMode == OcclusionMode.NEVER) {
      int textColor = entity.isDiscrete() ? DIMMED_TEXT_COLOR : VISIBLE_TEXT_COLOR;
      font.drawInBatch(
          displayName,
          textOffsetX,
          0.0f,
          textColor,
          false,
          pose,
          bufferSource,
          Font.DisplayMode.NORMAL,
          backgroundColor,
          packedLight);
      poseStack.popPose();
      return;
    }

    int seeThroughTextColor =
        occlusionMode == OcclusionMode.ALWAYS ? VISIBLE_TEXT_COLOR : DIMMED_TEXT_COLOR;
    font.drawInBatch(
        displayName,
        textOffsetX,
        0.0f,
        seeThroughTextColor,
        false,
        pose,
        bufferSource,
        Font.DisplayMode.SEE_THROUGH,
        backgroundColor,
        packedLight);

    if (occlusionMode == OcclusionMode.GHOST) {
      font.drawInBatch(
          displayName,
          textOffsetX,
          0.0f,
          VISIBLE_TEXT_COLOR,
          false,
          pose,
          bufferSource,
          Font.DisplayMode.NORMAL,
          0,
          packedLight);
    }

    poseStack.popPose();
  }
}
