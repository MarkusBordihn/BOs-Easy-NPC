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
import de.markusbordihn.easynpc.compat.iris.IrisManager;
import de.markusbordihn.easynpc.config.ClientSpeechBubbleConfig;
import de.markusbordihn.easynpc.config.SpeechBubbleOcclusionMode;
import de.markusbordihn.easynpc.data.action.SpeechBubbleManager;
import de.markusbordihn.easynpc.data.action.SpeechBubbleManager.SpeechBubbleEntry;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.client.Camera;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.LightTexture;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityAttachment;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.phys.Vec3;
import org.joml.Matrix4f;
import org.joml.Vector3f;

public class SpeechBubbleRenderer {

  private static final double DISCRETE_RENDER_DISTANCE_FACTOR = 0.5D;
  private static final float BODY_BORDER = 8.0F;
  private static final float BODY_SIZE = 32.0F;
  private static final float BUBBLE_DEPTH_BEHIND_TEXT = -0.03F;
  private static final float GHOST_TEXT_OPACITY_FACTOR = 1.4F;
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
  private static final int FULLY_VISIBLE_ALPHA = 255;
  private static final int TEXT_COLOR = 0x202020;
  private static final int TEXT_PACKED_LIGHT = LightTexture.pack(0, 0);

  private SpeechBubbleRenderer() {}

  public static void render(
      Entity entity, PoseStack poseStack, MultiBufferSource bufferSource, int packedLight) {
    SpeechBubbleEntry speechBubbleEntry = SpeechBubbleManager.get(entity.getUUID());
    if (speechBubbleEntry == null) {
      return;
    }

    Minecraft minecraft = Minecraft.getInstance();
    SpeechBubbleInstance speechBubbleInstance =
        measure(
            entity,
            speechBubbleEntry,
            minecraft,
            minecraft.getTimer().getGameTimeDeltaPartialTick(false),
            packedLight);
    if (speechBubbleInstance != null) {
      draw(speechBubbleInstance, poseStack, bufferSource, 0.0F, 0.0F);
    }
  }

  public static SpeechBubbleInstance measure(
      Entity entity,
      SpeechBubbleEntry speechBubbleEntry,
      Minecraft minecraft,
      float partialTick,
      int packedLight) {
    if (minecraft.player == null
        || entity.isInvisibleTo(minecraft.player)
        || (entity instanceof LivingEntity livingEntity && livingEntity.isDeadOrDying())) {
      return null;
    }

    double maxRenderDistance = ClientSpeechBubbleConfig.MAX_RENDER_DISTANCE_BLOCKS;
    if (entity.isDiscrete()) {
      maxRenderDistance *= DISCRETE_RENDER_DISTANCE_FACTOR;
    }
    if (minecraft.getEntityRenderDispatcher().distanceToSqr(entity)
        > maxRenderDistance * maxRenderDistance) {
      return null;
    }

    float opacity = speechBubbleEntry.getOpacity(SpeechBubbleManager.currentTick() + partialTick);
    int textAlpha = (int) (opacity * 255.0F);
    if (textAlpha < MIN_TEXT_ALPHA) {
      return null;
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
      return null;
    }

    Camera camera = minecraft.gameRenderer.getMainCamera();
    Vec3 entityPosition = entity.getPosition(partialTick);
    Vec3 cameraPosition = camera.getPosition();
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

    boolean hasDownwardTail = lateralProgress < LATERAL_TAIL_THRESHOLD;
    float localLeft =
        bubbleCenterX - bubbleWidth / 2.0F - (hasDownwardTail ? 0.0F : SIDE_TAIL_WIDTH);
    float localRight = bubbleCenterX + bubbleWidth / 2.0F;
    float localTop = bubbleCenterY - bubbleHeight / 2.0F;
    float localBottom =
        bubbleCenterY + bubbleHeight / 2.0F + (hasDownwardTail ? TAIL_HEIGHT : 0.0F);

    double anchorToCameraX = entityPosition.x + anchorOffsetX - cameraPosition.x;
    double anchorToCameraY = entityPosition.y + placedAnchorHeight - cameraPosition.y;
    double anchorToCameraZ = entityPosition.z + anchorOffsetZ - cameraPosition.z;
    Vector3f lookVector = camera.getLookVector();
    Vector3f upVector = camera.getUpVector();
    Vector3f leftVector = camera.getLeftVector();
    double cameraSpaceDepth =
        anchorToCameraX * lookVector.x()
            + anchorToCameraY * lookVector.y()
            + anchorToCameraZ * lookVector.z();
    double anchorSpaceUp =
        anchorToCameraX * upVector.x()
            + anchorToCameraY * upVector.y()
            + anchorToCameraZ * upVector.z();
    double anchorSpaceRight =
        -(anchorToCameraX * leftVector.x()
            + anchorToCameraY * leftVector.y()
            + anchorToCameraZ * leftVector.z());

    return new SpeechBubbleInstance(
        entity.getUUID(),
        lines,
        entityPosition,
        anchorOffsetX,
        anchorOffsetZ,
        placedAnchorHeight,
        bubbleScale,
        bubbleWidth,
        bubbleHeight,
        bubbleCenterX,
        bubbleCenterY,
        lateralProgress,
        anchorSpaceRight + bubbleScale * (localLeft + localRight) / 2.0D,
        anchorSpaceUp - bubbleScale * (localTop + localBottom) / 2.0D,
        cameraSpaceDepth,
        bubbleScale * (localRight - localLeft) / 2.0D,
        bubbleScale * (localBottom - localTop) / 2.0D,
        LightTexture.pack(
            Math.max(LightTexture.block(packedLight), ClientSpeechBubbleConfig.MIN_LIGHT_LEVEL),
            Math.max(LightTexture.sky(packedLight), ClientSpeechBubbleConfig.MIN_LIGHT_LEVEL)),
        textAlpha);
  }

  public static void draw(
      SpeechBubbleInstance speechBubbleInstance,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      float offsetPixelsX,
      float offsetPixelsY) {
    Minecraft minecraft = Minecraft.getInstance();
    float bubbleScale = speechBubbleInstance.bubbleScale();

    poseStack.pushPose();
    poseStack.translate(
        speechBubbleInstance.anchorOffsetX(),
        speechBubbleInstance.placedAnchorHeight(),
        speechBubbleInstance.anchorOffsetZ());
    poseStack.mulPose(minecraft.getEntityRenderDispatcher().cameraOrientation());
    poseStack.scale(bubbleScale, -bubbleScale, bubbleScale);

    drawBubble(
        speechBubbleInstance, poseStack.last().pose(), bufferSource, offsetPixelsX, offsetPixelsY);

    poseStack.popPose();
  }

  private static void drawBubble(
      SpeechBubbleInstance speechBubbleInstance,
      Matrix4f pose,
      MultiBufferSource bufferSource,
      float offsetPixelsX,
      float offsetPixelsY) {
    SpeechBubbleOcclusionMode occlusionMode = ClientSpeechBubbleConfig.OCCLUSION_MODE;
    if (IrisManager.isShaderPackInUse()) {
      drawDepthTestedBubble(
          speechBubbleInstance, pose, bufferSource, occlusionMode, offsetPixelsX, offsetPixelsY);
      return;
    }

    drawSeeThroughBubble(
        speechBubbleInstance, pose, bufferSource, occlusionMode, offsetPixelsX, offsetPixelsY);
  }

  private static void drawSeeThroughBubble(
      SpeechBubbleInstance speechBubbleInstance,
      Matrix4f pose,
      MultiBufferSource bufferSource,
      SpeechBubbleOcclusionMode occlusionMode,
      float offsetPixelsX,
      float offsetPixelsY) {
    int textAlpha = speechBubbleInstance.textAlpha();

    if (occlusionMode != SpeechBubbleOcclusionMode.NEVER) {
      int occludedBodyAlpha = occludedBodyAlpha(occlusionMode, textAlpha);
      drawBubbleBody(
          speechBubbleInstance,
          pose,
          bufferSource.getBuffer(RenderType.textSeeThrough(Constants.TEXTURE_SPEECH_BUBBLE)),
          occludedBodyAlpha,
          offsetPixelsX,
          offsetPixelsY);
      drawBubbleText(
          speechBubbleInstance,
          pose,
          bufferSource,
          Font.DisplayMode.SEE_THROUGH,
          Math.min(textAlpha, (int) (occludedBodyAlpha * GHOST_TEXT_OPACITY_FACTOR)),
          offsetPixelsX,
          offsetPixelsY);
    }

    if (occlusionMode != SpeechBubbleOcclusionMode.ALWAYS) {
      drawBubbleBody(
          speechBubbleInstance,
          pose,
          bufferSource.getBuffer(RenderType.text(Constants.TEXTURE_SPEECH_BUBBLE)),
          textAlpha,
          offsetPixelsX,
          offsetPixelsY);
      drawBubbleText(
          speechBubbleInstance,
          pose,
          bufferSource,
          Font.DisplayMode.NORMAL,
          textAlpha,
          offsetPixelsX,
          offsetPixelsY);
    }
  }

  private static void drawDepthTestedBubble(
      SpeechBubbleInstance speechBubbleInstance,
      Matrix4f pose,
      MultiBufferSource bufferSource,
      SpeechBubbleOcclusionMode occlusionMode,
      float offsetPixelsX,
      float offsetPixelsY) {
    int textAlpha = speechBubbleInstance.textAlpha();
    RenderType bodyRenderType = RenderType.text(Constants.TEXTURE_SPEECH_BUBBLE);
    MultiBufferSource textBufferSource = bufferSource;
    if (textAlpha == FULLY_VISIBLE_ALPHA) {
      bodyRenderType = SpeechBubbleRenderTypes.opaqueBody();
      textBufferSource = SpeechBubbleRenderTypes.opaqueTextBufferSource(bufferSource);
    }

    drawBubbleBody(
        speechBubbleInstance,
        pose,
        bufferSource.getBuffer(bodyRenderType),
        textAlpha,
        offsetPixelsX,
        offsetPixelsY);

    if (occlusionMode != SpeechBubbleOcclusionMode.NEVER) {
      int occludedBodyAlpha = occludedBodyAlpha(occlusionMode, textAlpha);
      drawBubbleBody(
          speechBubbleInstance,
          pose,
          bufferSource.getBuffer(SpeechBubbleRenderTypes.occluded()),
          occludedBodyAlpha,
          offsetPixelsX,
          offsetPixelsY);
      drawBubbleText(
          speechBubbleInstance,
          pose,
          bufferSource,
          Font.DisplayMode.SEE_THROUGH,
          Math.min(textAlpha, (int) (occludedBodyAlpha * GHOST_TEXT_OPACITY_FACTOR)),
          offsetPixelsX,
          offsetPixelsY);
    }

    drawBubbleText(
        speechBubbleInstance,
        pose,
        textBufferSource,
        Font.DisplayMode.NORMAL,
        textAlpha,
        offsetPixelsX,
        offsetPixelsY);
  }

  private static int occludedBodyAlpha(SpeechBubbleOcclusionMode occlusionMode, int textAlpha) {
    if (occlusionMode == SpeechBubbleOcclusionMode.GHOST) {
      return (int) (textAlpha * ClientSpeechBubbleConfig.GHOST_OPACITY / 100.0F);
    }

    return textAlpha;
  }

  private static void drawBubbleBody(
      SpeechBubbleInstance speechBubbleInstance,
      Matrix4f pose,
      VertexConsumer bubbleConsumer,
      int bodyAlpha,
      float offsetPixelsX,
      float offsetPixelsY) {
    float bubbleWidth = speechBubbleInstance.bubbleWidthPixels();
    float bubbleHeight = speechBubbleInstance.bubbleHeightPixels();
    float bubbleCenterX = speechBubbleInstance.bubbleCenterPixelsX() + offsetPixelsX;
    float bubbleCenterY = speechBubbleInstance.bubbleCenterPixelsY() + offsetPixelsY;
    float bubbleTop = bubbleCenterY - bubbleHeight / 2.0F;
    float bubbleLeft = bubbleCenterX - bubbleWidth / 2.0F;
    int packedLight = speechBubbleInstance.packedLight();

    renderBubbleBody(
        pose,
        bubbleConsumer,
        bubbleLeft,
        bubbleTop,
        bubbleWidth,
        bubbleHeight,
        packedLight,
        bodyAlpha);
    if (speechBubbleInstance.lateralProgress() < LATERAL_TAIL_THRESHOLD) {
      renderBubbleTail(
          pose, bubbleConsumer, bubbleCenterX, bubbleTop + bubbleHeight, packedLight, bodyAlpha);
    } else {
      renderSideBubbleTail(pose, bubbleConsumer, bubbleLeft, bubbleCenterY, packedLight, bodyAlpha);
    }
  }

  private static void drawBubbleText(
      SpeechBubbleInstance speechBubbleInstance,
      Matrix4f pose,
      MultiBufferSource bufferSource,
      Font.DisplayMode displayMode,
      int textAlpha,
      float offsetPixelsX,
      float offsetPixelsY) {
    float bubbleCenterX = speechBubbleInstance.bubbleCenterPixelsX() + offsetPixelsX;
    float bubbleCenterY = speechBubbleInstance.bubbleCenterPixelsY() + offsetPixelsY;

    Font font = Minecraft.getInstance().font;
    float lineY = bubbleCenterY - speechBubbleInstance.bubbleHeightPixels() / 2.0F + PADDING_Y;
    for (FormattedCharSequence line : speechBubbleInstance.lines()) {
      font.drawInBatch(
          line,
          bubbleCenterX - font.width(line) / 2.0F,
          lineY,
          TEXT_COLOR | (textAlpha << 24),
          false,
          pose,
          bufferSource,
          displayMode,
          0,
          TEXT_PACKED_LIGHT);
      lineY += LINE_HEIGHT;
    }
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
