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
import com.mojang.blaze3d.vertex.VertexConsumer;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.action.SpeechBubbleManager;
import de.markusbordihn.easynpc.data.action.SpeechBubbleManager.SpeechBubbleEntry;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.rendertype.RenderTypes;
import net.minecraft.client.renderer.state.CameraRenderState;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityAttachment;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.phys.Vec3;
import org.joml.Matrix4f;

public class SpeechBubbleRenderer {

  private static final double MAX_RENDER_DISTANCE = 64.0D;
  private static final double MAX_RENDER_DISTANCE_DISCRETE = 32.0D;
  private static final float BODY_BORDER = 8.0F;
  private static final float BODY_SIZE = 32.0F;
  private static final float BUBBLE_DEPTH_BEHIND_TEXT = -0.03F;
  private static final float LATERAL_TAIL_THRESHOLD = 0.5F;
  private static final float NAME_TAG_CLEARANCE = 0.3F;
  private static final float PADDING_X = 6.0F;
  private static final float PADDING_Y = 5.0F;
  private static final float SIDE_TAIL_HEIGHT = 16.0F;
  private static final float SIDE_TAIL_WIDTH = 8.0F;
  private static final float TAIL_HEIGHT = 8.0F;
  private static final float TAIL_U = 32.0F;
  private static final float TAIL_WIDTH = 16.0F;
  private static final float TEXTURE_SIZE = 64.0F;
  private static final int GLYPH_HEIGHT = 8;
  private static final int LINE_HEIGHT = 10;
  private static final int MAX_LINE_WIDTH = 160;
  private static final int MAX_LINES = 5;
  private static final int MIN_TEXT_ALPHA = 4;
  private static final int TEXT_COLOR = 0x202020;

  private SpeechBubbleRenderer() {}

  public static void submit(
      EntityRenderState renderState,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState) {
    if (!(renderState instanceof EasyNPCRenderStateExtension extension)
        || extension.getEasyNpcUUID() == null) {
      return;
    }

    EasyNPC<?> easyNPC =
        LivingEntityManager.getClientEasyNPCEntityByUUID(extension.getEasyNpcUUID());
    if (easyNPC != null) {
      submit(
          easyNPC.getEntity(),
          poseStack,
          submitNodeCollector,
          cameraRenderState,
          renderState.lightCoords);
    }
  }

  public static void submit(
      Entity entity,
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      CameraRenderState cameraRenderState,
      int packedLight) {
    SpeechBubbleEntry speechBubbleEntry = SpeechBubbleManager.get(entity.getUUID());
    if (speechBubbleEntry == null) {
      return;
    }

    Minecraft minecraft = Minecraft.getInstance();
    if (minecraft.player == null
        || entity.isInvisibleTo(minecraft.player)
        || (entity instanceof LivingEntity livingEntity && livingEntity.isDeadOrDying())) {
      return;
    }

    double maxRenderDistance =
        entity.isDiscrete() ? MAX_RENDER_DISTANCE_DISCRETE : MAX_RENDER_DISTANCE;
    if (minecraft.getEntityRenderDispatcher().distanceToSqr(entity)
        > maxRenderDistance * maxRenderDistance) {
      return;
    }

    float partialTick = minecraft.getDeltaTracker().getGameTimeDeltaPartialTick(false);
    float opacity = speechBubbleEntry.getOpacity(SpeechBubbleManager.currentTick() + partialTick);
    int textAlpha = (int) (opacity * 255.0F);
    if (textAlpha < MIN_TEXT_ALPHA) {
      return;
    }

    Font font = minecraft.font;
    List<FormattedCharSequence> lines = splitIntoLines(font, speechBubbleEntry.text());
    float contentWidth = 0.0F;
    for (FormattedCharSequence line : lines) {
      contentWidth = Math.max(contentWidth, font.width(line));
    }

    float contentHeight = (lines.size() - 1) * (float) LINE_HEIGHT + GLYPH_HEIGHT;
    float bubbleWidth = Math.max(contentWidth + 2.0F * PADDING_X, 2.0F * BODY_BORDER);
    float bubbleHeight = Math.max(contentHeight + 2.0F * PADDING_Y, 2.0F * BODY_BORDER);

    Vec3 nameTagOffset =
        entity
            .getAttachments()
            .getNullable(EntityAttachment.NAME_TAG, 0, entity.getViewYRot(partialTick));
    if (nameTagOffset == null) {
      return;
    }

    Vec3 entityPosition = entity.getPosition(partialTick);
    Vec3 cameraPosition = cameraRenderState.pos;
    float anchorHeight = (float) (nameTagOffset.y + 0.5D) + NAME_TAG_CLEARANCE;
    double entityToCameraX = cameraPosition.x - entityPosition.x;
    double entityToCameraZ = cameraPosition.z - entityPosition.z;
    double entityHorizontalDistanceToCamera = Math.hypot(entityToCameraX, entityToCameraZ);
    float cameraClearance =
        SpeechBubblePlacement.cameraClearance(
            entity.getBbWidth(), entityHorizontalDistanceToCamera);
    double clearanceRatio =
        entityHorizontalDistanceToCamera > 0.0D
            ? cameraClearance / entityHorizontalDistanceToCamera
            : 0.0D;
    double anchorOffsetX = nameTagOffset.x + entityToCameraX * clearanceRatio;
    double anchorOffsetZ = nameTagOffset.z + entityToCameraZ * clearanceRatio;
    double horizontalDistanceToCamera = entityHorizontalDistanceToCamera - cameraClearance;
    double distanceToCamera =
        Math.hypot(horizontalDistanceToCamera, entityPosition.y + anchorHeight - cameraPosition.y);

    int fieldOfViewDegrees = minecraft.options.fov().get();
    float scaleForDistance = SpeechBubblePlacement.scaleForDistance(distanceToCamera);
    double maxVerticalOffset =
        SpeechBubblePlacement.maxOffsetWithinLimit(
            horizontalDistanceToCamera,
            SpeechBubblePlacement.elevationLimitRadians(fieldOfViewDegrees));

    float extentAboveAnchor = TAIL_HEIGHT + bubbleHeight;
    float requiredAnchorDrop =
        SpeechBubblePlacement.requiredAnchorDrop(
            entityPosition.y
                + anchorHeight
                + extentAboveAnchor * scaleForDistance
                - cameraPosition.y,
            maxVerticalOffset);
    float maxAnchorDrop = Math.max(0.0F, anchorHeight - entity.getBbHeight());
    float lateralProgress =
        SpeechBubblePlacement.lateralProgress(requiredAnchorDrop, maxAnchorDrop);

    float placedAnchorHeight =
        SpeechBubblePlacement.lerp(
            lateralProgress,
            anchorHeight - Math.min(requiredAnchorDrop, maxAnchorDrop),
            entity.getEyeHeight());
    float bubbleCenterX =
        SpeechBubblePlacement.lerp(
            lateralProgress,
            0.0F,
            SpeechBubblePlacement.lateralClearancePixels(entity.getBbWidth(), scaleForDistance)
                + SIDE_TAIL_WIDTH
                + bubbleWidth / 2.0F);
    float bubbleCenterY =
        SpeechBubblePlacement.lerp(lateralProgress, -(TAIL_HEIGHT + bubbleHeight / 2.0F), 0.0F);

    float bubbleScale =
        SpeechBubblePlacement.fitScale(
            scaleForDistance,
            maxVerticalOffset - (entityPosition.y + placedAnchorHeight - cameraPosition.y),
            SpeechBubblePlacement.maxOffsetWithinLimit(
                horizontalDistanceToCamera,
                SpeechBubblePlacement.azimuthLimitRadians(
                    fieldOfViewDegrees,
                    minecraft.getWindow().getWidth(),
                    minecraft.getWindow().getHeight())),
            bubbleHeight / 2.0F - bubbleCenterY,
            bubbleWidth / 2.0F + bubbleCenterX);

    float bubbleTop = bubbleCenterY - bubbleHeight / 2.0F;
    float bubbleLeft = bubbleCenterX - bubbleWidth / 2.0F;

    poseStack.pushPose();
    poseStack.translate(anchorOffsetX, placedAnchorHeight, anchorOffsetZ);
    poseStack.mulPose(cameraRenderState.orientation);
    poseStack.scale(bubbleScale, -bubbleScale, bubbleScale);

    submitNodeCollector.submitCustomGeometry(
        poseStack,
        RenderTypes.text(Constants.TEXTURE_SPEECH_BUBBLE),
        (pose, consumer) -> {
          renderBubbleBody(
              pose.pose(),
              consumer,
              bubbleLeft,
              bubbleTop,
              bubbleWidth,
              bubbleHeight,
              packedLight,
              textAlpha);
          if (lateralProgress < LATERAL_TAIL_THRESHOLD) {
            renderBubbleTail(
                pose.pose(),
                consumer,
                bubbleCenterX,
                bubbleTop + bubbleHeight,
                packedLight,
                textAlpha);
          } else {
            renderSideBubbleTail(
                pose.pose(), consumer, bubbleLeft, bubbleCenterY, packedLight, textAlpha);
          }
        });

    float lineY = bubbleTop + PADDING_Y;
    for (FormattedCharSequence line : lines) {
      submitNodeCollector.submitText(
          poseStack,
          bubbleCenterX - font.width(line) / 2.0F,
          lineY,
          line,
          false,
          Font.DisplayMode.NORMAL,
          packedLight,
          TEXT_COLOR | (textAlpha << 24),
          0,
          0);
      lineY += LINE_HEIGHT;
    }

    poseStack.popPose();
  }

  private static List<FormattedCharSequence> splitIntoLines(Font font, Component text) {
    List<FormattedCharSequence> lines = font.split(text, MAX_LINE_WIDTH);
    if (lines.size() <= MAX_LINES) {
      return lines;
    }

    List<FormattedCharSequence> limitedLines = new ArrayList<>(lines.subList(0, MAX_LINES));
    int lastLine = MAX_LINES - 1;
    limitedLines.set(
        lastLine,
        FormattedCharSequence.composite(
            limitedLines.get(lastLine), Component.literal("…").getVisualOrderText()));

    return limitedLines;
  }

  private static void renderBubbleBody(
      Matrix4f pose,
      VertexConsumer consumer,
      float left,
      float top,
      float width,
      float height,
      int packedLight,
      int alpha) {
    float[] columns = {left, left + BODY_BORDER, left + width - BODY_BORDER, left + width};
    float[] rows = {top, top + BODY_BORDER, top + height - BODY_BORDER, top + height};
    float[] sharedAxisTexture = {0.0F, BODY_BORDER, BODY_SIZE - BODY_BORDER, BODY_SIZE};

    for (int column = 0; column < 3; column++) {
      for (int row = 0; row < 3; row++) {
        quad(
            pose,
            consumer,
            columns[column],
            rows[row],
            columns[column + 1],
            rows[row + 1],
            sharedAxisTexture[column],
            sharedAxisTexture[row],
            sharedAxisTexture[column + 1],
            sharedAxisTexture[row + 1],
            packedLight,
            alpha);
      }
    }
  }

  private static void renderBubbleTail(
      Matrix4f pose,
      VertexConsumer consumer,
      float centerX,
      float top,
      int packedLight,
      int alpha) {
    quad(
        pose,
        consumer,
        centerX - TAIL_WIDTH / 2.0F,
        top,
        centerX + TAIL_WIDTH / 2.0F,
        top + TAIL_HEIGHT,
        TAIL_U,
        0.0F,
        TAIL_U + TAIL_WIDTH,
        TAIL_HEIGHT,
        packedLight,
        alpha);
  }

  private static void renderSideBubbleTail(
      Matrix4f pose,
      VertexConsumer consumer,
      float right,
      float centerY,
      int packedLight,
      int alpha) {
    float left = right - SIDE_TAIL_WIDTH;
    float top = centerY - SIDE_TAIL_HEIGHT / 2.0F;
    float bottom = centerY + SIDE_TAIL_HEIGHT / 2.0F;
    float u0 = TAIL_U / TEXTURE_SIZE;
    float u1 = (TAIL_U + TAIL_WIDTH) / TEXTURE_SIZE;
    float v = TAIL_HEIGHT / TEXTURE_SIZE;

    // Same sprite as the downward tail, rotated by 90 degrees through the vertex order.
    vertex(pose, consumer, left, top, u0, v, packedLight, alpha);
    vertex(pose, consumer, left, bottom, u1, v, packedLight, alpha);
    vertex(pose, consumer, right, bottom, u1, 0.0F, packedLight, alpha);
    vertex(pose, consumer, right, top, u0, 0.0F, packedLight, alpha);
  }

  private static void quad(
      Matrix4f pose,
      VertexConsumer consumer,
      float x0,
      float y0,
      float x1,
      float y1,
      float textureX0,
      float textureY0,
      float textureX1,
      float textureY1,
      int packedLight,
      int alpha) {
    float u0 = textureX0 / TEXTURE_SIZE;
    float v0 = textureY0 / TEXTURE_SIZE;
    float u1 = textureX1 / TEXTURE_SIZE;
    float v1 = textureY1 / TEXTURE_SIZE;

    // Same vertex order as BakedGlyph, so the quad faces the camera like the text does.
    vertex(pose, consumer, x0, y0, u0, v0, packedLight, alpha);
    vertex(pose, consumer, x0, y1, u0, v1, packedLight, alpha);
    vertex(pose, consumer, x1, y1, u1, v1, packedLight, alpha);
    vertex(pose, consumer, x1, y0, u1, v0, packedLight, alpha);
  }

  private static void vertex(
      Matrix4f pose,
      VertexConsumer consumer,
      float x,
      float y,
      float u,
      float v,
      int packedLight,
      int alpha) {
    consumer
        .addVertex(pose, x, y, BUBBLE_DEPTH_BEHIND_TEXT)
        .setColor(255, 255, 255, alpha)
        .setUv(u, v)
        .setLight(packedLight);
  }
}
