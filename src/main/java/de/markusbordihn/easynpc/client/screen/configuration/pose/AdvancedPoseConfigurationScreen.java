/**
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

package de.markusbordihn.easynpc.client.screen.configuration.pose;

import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.core.Rotations;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.menu.configuration.pose.AdvancedPoseConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessage;

@OnlyIn(Dist.CLIENT)
public class AdvancedPoseConfigurationScreen
    extends PoseConfigurationScreen<AdvancedPoseConfigurationMenu> {

  // Internal
  protected Button resetArmsRotationButton;
  protected Button resetBodyRotationButton;
  protected Button resetHeadRotationButton;
  protected Button resetLeftArmRotationButton;
  protected Button resetLeftLegRotationButton;
  protected Button resetRightArmRotationButton;
  protected Button resetRightLegRotationButton;
  protected Checkbox showHeadCheckbox;
  protected SliderButton armsRotationXSliderButton;
  protected SliderButton armsRotationYSliderButton;
  protected SliderButton armsRotationZSliderButton;
  protected SliderButton bodyRotationXSliderButton;
  protected SliderButton bodyRotationYSliderButton;
  protected SliderButton bodyRotationZSliderButton;
  protected SliderButton headRotationXSliderButton;
  protected SliderButton headRotationYSliderButton;
  protected SliderButton headRotationZSliderButton;
  protected SliderButton leftArmRotationXSliderButton;
  protected SliderButton leftArmRotationYSliderButton;
  protected SliderButton leftArmRotationZSliderButton;
  protected SliderButton leftLegRotationXSliderButton;
  protected SliderButton leftLegRotationYSliderButton;
  protected SliderButton leftLegRotationZSliderButton;
  protected SliderButton rightArmRotationXSliderButton;
  protected SliderButton rightArmRotationYSliderButton;
  protected SliderButton rightArmRotationZSliderButton;
  protected SliderButton rightLegRotationXSliderButton;
  protected SliderButton rightLegRotationYSliderButton;
  protected SliderButton rightLegRotationZSliderButton;

  // Rotations
  private float armsRotationX = 0f;
  private float armsRotationY = 0f;
  private float armsRotationZ = 0f;
  private float bodyRotationX = 0f;
  private float bodyRotationY = 0f;
  private float bodyRotationZ = 0f;
  private float headRotationX = 0f;
  private float headRotationY = 0f;
  private float headRotationZ = 0f;
  private float leftArmRotationX = 0f;
  private float leftArmRotationY = 0f;
  private float leftArmRotationZ = 0f;
  private float leftLegRotationX = 0f;
  private float leftLegRotationY = 0f;
  private float leftLegRotationZ = 0f;
  private float rightArmRotationX = 0f;
  private float rightArmRotationY = 0f;
  private float rightArmRotationZ = 0f;
  private float rightLegRotationX = 0f;
  private float rightLegRotationY = 0f;
  private float rightLegRotationZ = 0f;

  public AdvancedPoseConfigurationScreen(AdvancedPoseConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  private static Button menuButton(int left, int top, String label, Button.OnPress onPress) {
    return menuButton(left, top, 90, "pose." + label, onPress);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.advancedPoseButton.active = false;

    // Position and size
    int scaleWidth = 30;
    int sliderTopPos = this.contentTopPos + 16;
    int sliderLeftPos = this.contentLeftPos;

    // Head parts
    if (this.entity.hasHeadModelPart()) {

      // Head rotations
      Rotations headRotations = this.entity.getModelHeadRotation();
      this.headRotationXSliderButton = this.addRenderableWidget(
          new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20, " headRotationX",
              (float) Math.toDegrees(headRotations.getX()), SliderButton.Type.DEGREE, slider -> {
                this.headRotationX = (float) Math.toRadians(slider.getTargetValue());
                NetworkMessage.rotationChange(uuid, ModelPart.HEAD,
                    new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
              }));
      this.headRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.headRotationXSliderButton.getX() + this.headRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " headRotationY",
          (float) Math.toDegrees(headRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.headRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.HEAD,
                new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
          }));
      this.headRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.headRotationYSliderButton.getX() + this.headRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " headRotationZ",
          (float) Math.toDegrees(headRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.headRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.HEAD,
                new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
          }));
      this.resetHeadRotationButton = this.addRenderableWidget(
          menuButton(this.headRotationXSliderButton.getX(), sliderTopPos + 20, "reset", button -> {
            this.headRotationXSliderButton.reset();
            this.headRotationYSliderButton.reset();
            this.headRotationZSliderButton.reset();
            this.headRotationX = 0f;
            this.headRotationY = 0f;
            this.headRotationZ = 0f;
            NetworkMessage.rotationChange(uuid, ModelPart.HEAD,
                new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
          }));
    }

    // Body parts
    if (this.entity.hasBodyModelPart()) {
      sliderLeftPos += 191;

      // Body rotations
      Rotations bodyRotations = this.entity.getModelBodyRotation();
      this.bodyRotationXSliderButton = this.addRenderableWidget(
          new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20, " bodyRotationX",
              (float) Math.toDegrees(bodyRotations.getX()), SliderButton.Type.DEGREE, slider -> {
                this.bodyRotationX = (float) Math.toRadians(slider.getTargetValue());
                NetworkMessage.rotationChange(uuid, ModelPart.BODY,
                    new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
              }));
      this.bodyRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.bodyRotationXSliderButton.getX() + this.bodyRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " bodyRotationY",
          (float) Math.toDegrees(bodyRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.bodyRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.BODY,
                new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
          }));
      this.bodyRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.bodyRotationYSliderButton.getX() + this.bodyRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " bodyRotationZ",
          (float) Math.toDegrees(bodyRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.bodyRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.BODY,
                new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
          }));
      this.resetBodyRotationButton = this.addRenderableWidget(
          menuButton(this.bodyRotationXSliderButton.getX(), sliderTopPos + 20, "reset", button -> {
            this.bodyRotationXSliderButton.reset();
            this.bodyRotationYSliderButton.reset();
            this.bodyRotationZSliderButton.reset();
            this.bodyRotationX = 0f;
            this.bodyRotationY = 0f;
            this.bodyRotationZ = 0f;
            NetworkMessage.rotationChange(uuid, ModelPart.BODY,
                new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
          }));
    }

    sliderTopPos += 66;

    // Right arm rotations
    if (this.entity.hasRightArmModelPart()) {
      sliderLeftPos = this.contentLeftPos;
      Rotations rightArmRotations = this.entity.getModelRightArmRotation();
      this.rightArmRotationXSliderButton = this.addRenderableWidget(new SliderButton(sliderLeftPos,
          sliderTopPos, scaleWidth, 20, " rightArmRotationX",
          (float) Math.toDegrees(rightArmRotations.getX()), SliderButton.Type.DEGREE, slider -> {
            this.rightArmRotationX = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
          }));
      this.rightArmRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightArmRotationXSliderButton.getX() + this.rightArmRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightArmRotationY",
          (float) Math.toDegrees(rightArmRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.rightArmRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
          }));
      this.rightArmRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightArmRotationYSliderButton.getX() + this.rightArmRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightArmRotationZ",
          (float) Math.toDegrees(rightArmRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.rightArmRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
          }));
      this.resetRightArmRotationButton =
          this.addRenderableWidget(menuButton(this.rightArmRotationXSliderButton.getX(),
              sliderTopPos + 20, "reset", button -> {
                this.rightArmRotationXSliderButton.reset();
                this.rightArmRotationYSliderButton.reset();
                this.rightArmRotationZSliderButton.reset();
                this.rightArmRotationX = 0f;
                this.rightArmRotationY = 0f;
                this.rightArmRotationZ = 0f;
                NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                    this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
              }));
    }

    // Left arm rotations and arms rotations.
    if (this.entity.hasLeftArmModelPart()) {
      sliderLeftPos += 191;
      Rotations leftArmRotations = this.entity.getModelLeftArmRotation();
      this.leftArmRotationXSliderButton = this.addRenderableWidget(
          new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20, " leftArmRotationX",
              (float) Math.toDegrees(leftArmRotations.getX()), SliderButton.Type.DEGREE, slider -> {
                this.leftArmRotationX = (float) Math.toRadians(slider.getTargetValue());
                NetworkMessage.rotationChange(uuid, ModelPart.LEFT_ARM, new Rotations(
                    this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
              }));
      this.leftArmRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftArmRotationXSliderButton.getX() + this.leftArmRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftArmRotationY",
          (float) Math.toDegrees(leftArmRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.leftArmRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_ARM,
                new Rotations(this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
          }));
      this.leftArmRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftArmRotationYSliderButton.getX() + this.leftArmRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftArmRotationZ",
          (float) Math.toDegrees(leftArmRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.leftArmRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_ARM,
                new Rotations(this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
          }));
      this.resetLeftArmRotationButton =
          this.addRenderableWidget(menuButton(this.leftArmRotationXSliderButton.getX(),
              sliderTopPos + 20, "reset", button -> {
                this.leftArmRotationXSliderButton.reset();
                this.leftArmRotationYSliderButton.reset();
                this.leftArmRotationZSliderButton.reset();
                this.leftArmRotationX = 0f;
                this.leftArmRotationY = 0f;
                this.leftArmRotationZ = 0f;
                NetworkMessage.rotationChange(uuid, ModelPart.LEFT_ARM, new Rotations(
                    this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
              }));
    } else if (this.entity.hasArmsModelPart()) {
      sliderLeftPos = this.contentLeftPos;
      Rotations armsRotations = this.entity.getModelArmsRotation();
      this.armsRotationXSliderButton = this.addRenderableWidget(
          new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20, " armsRotationX",
              (float) Math.toDegrees(armsRotations.getX()), SliderButton.Type.DEGREE, slider -> {
                this.armsRotationX = (float) Math.toRadians(slider.getTargetValue());
                NetworkMessage.rotationChange(uuid, ModelPart.ARMS,
                    new Rotations(this.armsRotationX, this.armsRotationY, this.armsRotationZ));
              }));
      this.armsRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.armsRotationXSliderButton.getX() + this.armsRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " armsRotationY",
          (float) Math.toDegrees(armsRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.armsRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.ARMS,
                new Rotations(this.armsRotationX, this.armsRotationY, this.armsRotationZ));
          }));
      this.armsRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.armsRotationYSliderButton.getX() + this.armsRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " armsRotationZ",
          (float) Math.toDegrees(armsRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.armsRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.ARMS,
                new Rotations(this.armsRotationX, this.armsRotationY, this.armsRotationZ));
          }));
      this.resetArmsRotationButton = this.addRenderableWidget(
          menuButton(this.armsRotationXSliderButton.getX(), sliderTopPos + 20, "reset", button -> {
            this.armsRotationXSliderButton.reset();
            this.armsRotationYSliderButton.reset();
            this.armsRotationZSliderButton.reset();
            this.armsRotationX = 0f;
            this.armsRotationY = 0f;
            this.armsRotationZ = 0f;
            NetworkMessage.rotationChange(uuid, ModelPart.ARMS,
                new Rotations(this.armsRotationX, this.armsRotationY, this.armsRotationZ));
          }));
    }

    sliderTopPos += 66;
    sliderLeftPos = this.contentLeftPos;

    // Right leg rotations
    if (this.entity.hasRightLegModelPart()) {
      Rotations rightLegRotations = this.entity.getModelRightLegRotation();
      this.rightLegRotationXSliderButton = this.addRenderableWidget(new SliderButton(sliderLeftPos,
          sliderTopPos, scaleWidth, 20, " rightLegRotationX",
          (float) Math.toDegrees(rightLegRotations.getX()), SliderButton.Type.DEGREE, slider -> {
            this.rightLegRotationX = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
          }));
      this.rightLegRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightLegRotationXSliderButton.getX() + this.rightLegRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightLegRotationY",
          (float) Math.toDegrees(rightLegRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.rightLegRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
          }));
      this.rightLegRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightLegRotationYSliderButton.getX() + this.rightLegRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightLegRotationZ",
          (float) Math.toDegrees(rightLegRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.rightLegRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
          }));
      this.resetRightLegRotationButton =
          this.addRenderableWidget(menuButton(this.rightLegRotationXSliderButton.getX(),
              sliderTopPos + 20, "reset", button -> {
                this.rightLegRotationXSliderButton.reset();
                this.rightLegRotationYSliderButton.reset();
                this.rightLegRotationZSliderButton.reset();
                this.rightLegRotationX = 0f;
                this.rightLegRotationY = 0f;
                this.rightLegRotationZ = 0f;
                NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                    this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
              }));
    }

    // Left leg rotations
    if (this.entity.hasLeftLegModelPart()) {
      sliderLeftPos += 191;
      Rotations leftLegRotations = this.entity.getModelLeftLegRotation();
      this.leftLegRotationXSliderButton = this.addRenderableWidget(
          new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20, " leftLegRotationX",
              (float) Math.toDegrees(leftLegRotations.getX()), SliderButton.Type.DEGREE, slider -> {
                this.leftLegRotationX = (float) Math.toRadians(slider.getTargetValue());
                NetworkMessage.rotationChange(uuid, ModelPart.LEFT_LEG, new Rotations(
                    this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
              }));
      this.leftLegRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftLegRotationXSliderButton.getX() + this.leftLegRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftLegRotationY",
          (float) Math.toDegrees(leftLegRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.leftLegRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_LEG,
                new Rotations(this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
          }));
      this.leftLegRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftLegRotationYSliderButton.getX() + this.leftLegRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftLegRotationZ",
          (float) Math.toDegrees(leftLegRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.leftLegRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_LEG,
                new Rotations(this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
          }));
      this.resetLeftLegRotationButton =
          this.addRenderableWidget(menuButton(this.leftLegRotationXSliderButton.getX(),
              sliderTopPos + 20, "reset", button -> {
                this.leftLegRotationXSliderButton.reset();
                this.leftLegRotationYSliderButton.reset();
                this.leftLegRotationZSliderButton.reset();
                this.leftLegRotationX = 0f;
                this.leftLegRotationY = 0f;
                this.leftLegRotationZ = 0f;
                NetworkMessage.rotationChange(uuid, ModelPart.LEFT_LEG, new Rotations(
                    this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
              }));
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderCustomPoseEntityAvatar(this.contentLeftPos + 142, this.contentTopPos + 155,
        50, this.contentLeftPos + 140 - this.xMouse, this.contentTopPos + 30 - this.yMouse,
        this.entity);

    // Body parts texts
    if (this.entity.hasHeadModelPart()) {
      guiGraphics.drawString(this.font, "pose.head", this.headRotationXSliderButton.getX() + 5,
          this.headRotationXSliderButton.getY() - 12, Constants.FONT_COLOR_GRAY);
    }
    if (this.entity.hasBodyModelPart()) {
      guiGraphics.drawString(this.font, "pose.body", this.bodyRotationXSliderButton.getX() + 5,
          this.bodyRotationXSliderButton.getY() - 12, Constants.FONT_COLOR_GRAY);
    }
    if (this.entity.hasLeftArmModelPart()) {
      guiGraphics.drawString(this.font, "pose.left_arm",
          this.leftArmRotationXSliderButton.getX() + 5,
          this.leftArmRotationXSliderButton.getY() - 12, Constants.FONT_COLOR_GRAY);
    } else if (this.entity.hasArmsModelPart()) {
      guiGraphics.drawString(this.font, "pose.arms", this.armsRotationXSliderButton.getX() + 5,
          this.armsRotationXSliderButton.getY() - 12, Constants.FONT_COLOR_GRAY);
    }
    if (this.entity.hasRightArmModelPart()) {
      guiGraphics.drawString(this.font, "pose.right_arm",
          this.rightArmRotationXSliderButton.getX() + 5,
          this.rightArmRotationXSliderButton.getY() - 12, Constants.FONT_COLOR_GRAY);
    }
    if (this.entity.hasLeftLegModelPart()) {
      guiGraphics.drawString(this.font, "pose.left_leg",
          this.leftLegRotationXSliderButton.getX() + 5,
          this.leftLegRotationXSliderButton.getY() - 12, Constants.FONT_COLOR_GRAY);
    }
    if (this.entity.hasRightLegModelPart()) {
      guiGraphics.drawString(this.font, "pose.right_leg",
          this.rightLegRotationXSliderButton.getX() + 5,
          this.rightLegRotationXSliderButton.getY() - 12, Constants.FONT_COLOR_GRAY);
    }
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    // Entity background
    guiGraphics.fill(this.contentLeftPos + 90, this.contentTopPos, this.contentLeftPos + 191,
        this.contentTopPos + 187, 0xff000000);
    guiGraphics.fill(this.contentLeftPos + 91, this.contentTopPos + 1, this.contentLeftPos + 190,
        this.contentTopPos + 186, 0xffaaaaaa);
  }
}