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
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.config.ClientNameTagConfig;
import de.markusbordihn.easynpc.config.OcclusionMode;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.VisibilityHandler;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.client.renderer.LightTexture;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.state.CameraRenderState;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;

public class NameTagRenderer {

  private static final float NAME_TAG_SCALE = 0.025f;
  private static final float BACKGROUND_OPACITY = 0.25f;
  private static final int NAME_TAG_EMISSION = 2;
  private static final int DIMMED_TEXT_COLOR = -2130706433;
  private static final int VISIBLE_TEXT_COLOR = -1;

  private NameTagRenderer() {}

  public static void submit(
      final EntityRenderState renderState,
      final PoseStack poseStack,
      final SubmitNodeCollector submitNodeCollector,
      final CameraRenderState cameraRenderState) {
    Component displayName = renderState.nameTag;
    Vec3 nameTagAttachment = renderState.nameTagAttachment;
    if (displayName == null || nameTagAttachment == null || !isVisible(renderState)) {
      return;
    }

    Minecraft minecraft = Minecraft.getInstance();
    Font font = minecraft.font;
    FormattedCharSequence text = displayName.getVisualOrderText();
    float textOffsetX = -font.width(displayName) / 2.0f;
    int backgroundColor =
        (int) (minecraft.options.getBackgroundOpacity(BACKGROUND_OPACITY) * 255.0f) << 24;
    OcclusionMode occlusionMode =
        renderState.isDiscrete ? OcclusionMode.NEVER : ClientNameTagConfig.OCCLUSION_MODE;

    poseStack.pushPose();
    poseStack.translate(nameTagAttachment.x, nameTagAttachment.y + 0.5d, nameTagAttachment.z);
    poseStack.mulPose(cameraRenderState.orientation);
    poseStack.scale(NAME_TAG_SCALE, -NAME_TAG_SCALE, NAME_TAG_SCALE);

    if (occlusionMode == OcclusionMode.NEVER) {
      submitText(
          submitNodeCollector,
          poseStack,
          text,
          textOffsetX,
          Font.DisplayMode.NORMAL,
          renderState.isDiscrete ? DIMMED_TEXT_COLOR : VISIBLE_TEXT_COLOR,
          backgroundColor,
          renderState.isDiscrete
              ? renderState.lightCoords
              : LightTexture.lightCoordsWithEmission(renderState.lightCoords, NAME_TAG_EMISSION));
      poseStack.popPose();
      return;
    }

    submitText(
        submitNodeCollector,
        poseStack,
        text,
        textOffsetX,
        Font.DisplayMode.SEE_THROUGH,
        occlusionMode == OcclusionMode.ALWAYS ? VISIBLE_TEXT_COLOR : DIMMED_TEXT_COLOR,
        backgroundColor,
        renderState.lightCoords);

    if (occlusionMode == OcclusionMode.GHOST) {
      submitText(
          submitNodeCollector,
          poseStack,
          text,
          textOffsetX,
          Font.DisplayMode.NORMAL,
          VISIBLE_TEXT_COLOR,
          0,
          LightTexture.lightCoordsWithEmission(renderState.lightCoords, NAME_TAG_EMISSION));
    }

    poseStack.popPose();
  }

  private static void submitText(
      final SubmitNodeCollector submitNodeCollector,
      final PoseStack poseStack,
      final FormattedCharSequence text,
      final float textOffsetX,
      final Font.DisplayMode displayMode,
      final int textColor,
      final int backgroundColor,
      final int packedLight) {
    submitNodeCollector.submitText(
        poseStack,
        textOffsetX,
        0.0f,
        text,
        false,
        displayMode,
        packedLight,
        textColor,
        backgroundColor,
        0);
  }

  private static boolean isVisible(final EntityRenderState renderState) {
    double maxRenderDistance = ClientNameTagConfig.MAX_RENDER_DISTANCE_BLOCKS;
    if (renderState.distanceToCameraSq > maxRenderDistance * maxRenderDistance) {
      return false;
    }

    if (!(renderState instanceof EasyNPCRenderStateExtension extension)) {
      return true;
    }

    EasyNPC<?> easyNPC = EasyNPCModel.getEasyNPC(extension);
    LocalPlayer player = Minecraft.getInstance().player;
    if (easyNPC == null || player == null) {
      return true;
    }

    Entity entity = easyNPC.getEntity();
    return VisibilityHandler.handleIsCustomNameVisibleToPlayer(
        easyNPC, player, entity.isCustomNameVisible(), player.distanceToSqr(entity));
  }
}
