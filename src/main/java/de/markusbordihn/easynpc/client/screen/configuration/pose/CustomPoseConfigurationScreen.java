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

import net.minecraft.client.gui.components.Button;
import net.minecraft.core.Rotations;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import com.mojang.blaze3d.vertex.PoseStack;

import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.menu.configuration.pose.CustomPoseConfigurationMenu;
import de.markusbordihn.easynpc.model.ModelPart;
import de.markusbordihn.easynpc.network.NetworkHandler;
import de.markusbordihn.easynpc.skin.SkinModel;

@OnlyIn(Dist.CLIENT)
public class CustomPoseConfigurationScreen
    extends PoseConfigurationScreen<CustomPoseConfigurationMenu> {

  // Internal
  protected SliderButton headRotationXSliderButton;
  protected SliderButton headRotationYSliderButton;
  protected SliderButton headRotationZSliderButton;
  protected SliderButton bodyRotationXSliderButton;
  protected SliderButton bodyRotationYSliderButton;
  protected SliderButton bodyRotationZSliderButton;
  protected SliderButton leftArmRotationXSliderButton;
  protected SliderButton leftArmRotationYSliderButton;
  protected SliderButton leftArmRotationZSliderButton;
  protected SliderButton rightArmRotationXSliderButton;
  protected SliderButton rightArmRotationYSliderButton;
  protected SliderButton rightArmRotationZSliderButton;
  protected SliderButton leftLegRotationXSliderButton;
  protected SliderButton leftLegRotationYSliderButton;
  protected SliderButton leftLegRotationZSliderButton;
  protected SliderButton rightLegRotationXSliderButton;
  protected SliderButton rightLegRotationYSliderButton;
  protected SliderButton rightLegRotationZSliderButton;
  protected Button resetHeadRotationButton;
  protected Button resetBodyRotationButton;
  protected Button resetLeftArmRotationButton;
  protected Button resetRightArmRotationButton;
  protected Button resetLeftLegRotationButton;
  protected Button resetRightLegRotationButton;

  // Rotations
  private float headRotationX = 0f;
  private float headRotationY = 0f;
  private float headRotationZ = 0f;
  private float bodyRotationX = 0f;
  private float bodyRotationY = 0f;
  private float bodyRotationZ = 0f;
  private float leftArmRotationX = 0f;
  private float leftArmRotationY = 0f;
  private float leftArmRotationZ = 0f;
  private float rightArmRotationX = 0f;
  private float rightArmRotationY = 0f;
  private float rightArmRotationZ = 0f;
  private float leftLegRotationX = 0f;
  private float leftLegRotationY = 0f;
  private float leftLegRotationZ = 0f;
  private float rightLegRotationX = 0f;
  private float rightLegRotationY = 0f;
  private float rightLegRotationZ = 0f;

  public CustomPoseConfigurationScreen(CustomPoseConfigurationMenu menu, Inventory inventory,
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
    this.defaultPoseButton.active = true;
    this.customPoseButton.active = false;

    // Position and size
    int scaleWidth = 30;

    // Head rotations
    int sliderTopPos = this.contentTopPos + 13;
    int sliderLeftPos = this.contentLeftPos;
    Rotations headRotations = this.entity.getModelHeadRotation();
    this.headRotationXSliderButton =
        this.addRenderableWidget(new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20,
            " headRotationX", (float) Math.toDegrees(headRotations.getX()), -180f, 180f, slider -> {
              this.headRotationX = (float) Math.toRadians(slider.getTargetValue());
              NetworkHandler.rotationChange(uuid, ModelPart.HEAD,
                  new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
            }));
    this.headRotationYSliderButton = this.addRenderableWidget(new SliderButton(
        this.headRotationXSliderButton.x + this.headRotationXSliderButton.getWidth(), sliderTopPos,
        scaleWidth, 20, " headRotationY", (float) Math.toDegrees(headRotations.getY()), -180f, 180f,
        slider -> {
          this.headRotationY = (float) Math.toRadians(slider.getTargetValue());
          NetworkHandler.rotationChange(uuid, ModelPart.HEAD,
              new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
        }));
    this.headRotationZSliderButton = this.addRenderableWidget(new SliderButton(
        this.headRotationYSliderButton.x + this.headRotationYSliderButton.getWidth(), sliderTopPos,
        scaleWidth, 20, " headRotationZ", (float) Math.toDegrees(headRotations.getZ()), -180f, 180f,
        slider -> {
          this.headRotationZ = (float) Math.toRadians(slider.getTargetValue());
          NetworkHandler.rotationChange(uuid, ModelPart.HEAD,
              new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
        }));
    this.resetHeadRotationButton = this.addRenderableWidget(
        menuButton(this.headRotationXSliderButton.x, sliderTopPos + 20, "reset", button -> {
          this.headRotationXSliderButton.reset();
          this.headRotationYSliderButton.reset();
          this.headRotationZSliderButton.reset();
          this.headRotationX = 0f;
          this.headRotationY = 0f;
          this.headRotationZ = 0f;
          NetworkHandler.rotationChange(uuid, ModelPart.HEAD,
              new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
        }));

    // Body rotations
    sliderLeftPos += 191;
    Rotations bodyRotations = this.entity.getModelBodyRotation();
    this.bodyRotationXSliderButton =
        this.addRenderableWidget(new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20,
            " bodyRotationX", (float) Math.toDegrees(bodyRotations.getX()), -180f, 180f, slider -> {
              this.bodyRotationX = (float) Math.toRadians(slider.getTargetValue());
              NetworkHandler.rotationChange(uuid, ModelPart.BODY,
                  new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
            }));
    this.bodyRotationYSliderButton = this.addRenderableWidget(new SliderButton(
        this.bodyRotationXSliderButton.x + this.bodyRotationXSliderButton.getWidth(), sliderTopPos,
        scaleWidth, 20, " bodyRotationY", (float) Math.toDegrees(bodyRotations.getY()), -180f, 180f,
        slider -> {
          this.bodyRotationY = (float) Math.toRadians(slider.getTargetValue());
          NetworkHandler.rotationChange(uuid, ModelPart.BODY,
              new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
        }));
    this.bodyRotationZSliderButton = this.addRenderableWidget(new SliderButton(
        this.bodyRotationYSliderButton.x + this.bodyRotationYSliderButton.getWidth(), sliderTopPos,
        scaleWidth, 20, " bodyRotationZ", (float) Math.toDegrees(bodyRotations.getZ()), -180f, 180f,
        slider -> {
          this.bodyRotationZ = (float) Math.toRadians(slider.getTargetValue());
          NetworkHandler.rotationChange(uuid, ModelPart.BODY,
              new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
        }));
    this.resetBodyRotationButton = this.addRenderableWidget(
        menuButton(this.bodyRotationXSliderButton.x, sliderTopPos + 20, "reset", button -> {
          this.bodyRotationXSliderButton.reset();
          this.bodyRotationYSliderButton.reset();
          this.bodyRotationZSliderButton.reset();
          this.bodyRotationX = 0f;
          this.bodyRotationY = 0f;
          this.bodyRotationZ = 0f;
          NetworkHandler.rotationChange(uuid, ModelPart.BODY,
              new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
        }));

    sliderTopPos += 60;
    if (this.skinModel != SkinModel.VILLAGER) {
      // Right arm rotations
      sliderLeftPos = this.contentLeftPos;
      Rotations rightArmRotations = this.entity.getModelRightArmRotation();
      this.rightArmRotationXSliderButton = this.addRenderableWidget(
          new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20, " rightArmRotationX",
              (float) Math.toDegrees(rightArmRotations.getX()), -180f, 180f, slider -> {
                this.rightArmRotationX = (float) Math.toRadians(slider.getTargetValue());
                NetworkHandler.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                    this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
              }));
      this.rightArmRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightArmRotationXSliderButton.x + this.rightArmRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightArmRotationY",
          (float) Math.toDegrees(rightArmRotations.getY()), -180f, 180f, slider -> {
            this.rightArmRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkHandler.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
          }));
      this.rightArmRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightArmRotationYSliderButton.x + this.rightArmRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightArmRotationZ",
          (float) Math.toDegrees(rightArmRotations.getZ()), -180f, 180f, slider -> {
            this.rightArmRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkHandler.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
          }));
      this.resetRightArmRotationButton = this.addRenderableWidget(
          menuButton(this.rightArmRotationXSliderButton.x, sliderTopPos + 20, "reset", button -> {
            this.rightArmRotationXSliderButton.reset();
            this.rightArmRotationYSliderButton.reset();
            this.rightArmRotationZSliderButton.reset();
            this.rightArmRotationX = 0f;
            this.rightArmRotationY = 0f;
            this.rightArmRotationZ = 0f;
            NetworkHandler.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
          }));

      // Left arm rotations
      sliderLeftPos += 191;
      Rotations leftArmRotations = this.entity.getModelLeftArmRotation();
      this.leftArmRotationXSliderButton = this.addRenderableWidget(
          new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20, " leftArmRotationX",
              (float) Math.toDegrees(leftArmRotations.getX()), -180f, 180f, slider -> {
                this.leftArmRotationX = (float) Math.toRadians(slider.getTargetValue());
                NetworkHandler.rotationChange(uuid, ModelPart.LEFT_ARM, new Rotations(
                    this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
              }));
      this.leftArmRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftArmRotationXSliderButton.x + this.leftArmRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftArmRotationY",
          (float) Math.toDegrees(leftArmRotations.getY()), -180f, 180f, slider -> {
            this.leftArmRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkHandler.rotationChange(uuid, ModelPart.LEFT_ARM,
                new Rotations(this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
          }));
      this.leftArmRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftArmRotationYSliderButton.x + this.leftArmRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftArmRotationZ",
          (float) Math.toDegrees(leftArmRotations.getZ()), -180f, 180f, slider -> {
            this.leftArmRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkHandler.rotationChange(uuid, ModelPart.LEFT_ARM,
                new Rotations(this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
          }));
      this.resetLeftArmRotationButton = this.addRenderableWidget(
          menuButton(this.leftArmRotationXSliderButton.x, sliderTopPos + 20, "reset", button -> {
            this.leftArmRotationXSliderButton.reset();
            this.leftArmRotationYSliderButton.reset();
            this.leftArmRotationZSliderButton.reset();
            this.leftArmRotationX = 0f;
            this.leftArmRotationY = 0f;
            this.leftArmRotationZ = 0f;
            NetworkHandler.rotationChange(uuid, ModelPart.LEFT_ARM,
                new Rotations(this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
          }));
    }

    if (this.skinModel != SkinModel.ALLAY) {
      // Right leg rotations
      sliderTopPos += 60;
      sliderLeftPos = this.contentLeftPos;
      Rotations rightLegRotations = this.entity.getModelRightLegRotation();
      this.rightLegRotationXSliderButton = this.addRenderableWidget(
          new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20, " rightLegRotationX",
              (float) Math.toDegrees(rightLegRotations.getX()), -180f, 180f, slider -> {
                this.rightLegRotationX = (float) Math.toRadians(slider.getTargetValue());
                NetworkHandler.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                    this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
              }));
      this.rightLegRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightLegRotationXSliderButton.x + this.rightLegRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightLegRotationY",
          (float) Math.toDegrees(rightLegRotations.getY()), -180f, 180f, slider -> {
            this.rightLegRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkHandler.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
          }));
      this.rightLegRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightLegRotationYSliderButton.x + this.rightLegRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightLegRotationZ",
          (float) Math.toDegrees(rightLegRotations.getZ()), -180f, 180f, slider -> {
            this.rightLegRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkHandler.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
          }));
      this.resetRightLegRotationButton = this.addRenderableWidget(
          menuButton(this.rightLegRotationXSliderButton.x, sliderTopPos + 20, "reset", button -> {
            this.rightLegRotationXSliderButton.reset();
            this.rightLegRotationYSliderButton.reset();
            this.rightLegRotationZSliderButton.reset();
            this.rightLegRotationX = 0f;
            this.rightLegRotationY = 0f;
            this.rightLegRotationZ = 0f;
            NetworkHandler.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
          }));

      if (this.skinModel != SkinModel.FAIRY) {
        // Left leg rotations
        sliderLeftPos += 191;
        Rotations leftLegRotations = this.entity.getModelLeftLegRotation();
        this.leftLegRotationXSliderButton = this.addRenderableWidget(
            new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20, " leftLegRotationX",
                (float) Math.toDegrees(leftLegRotations.getX()), -180f, 180f, slider -> {
                  this.leftLegRotationX = (float) Math.toRadians(slider.getTargetValue());
                  NetworkHandler.rotationChange(uuid, ModelPart.LEFT_LEG, new Rotations(
                      this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
                }));
        this.leftLegRotationYSliderButton = this.addRenderableWidget(new SliderButton(
            this.leftLegRotationXSliderButton.x + this.leftLegRotationXSliderButton.getWidth(),
            sliderTopPos, scaleWidth, 20, " leftLegRotationY",
            (float) Math.toDegrees(leftLegRotations.getY()), -180f, 180f, slider -> {
              this.leftLegRotationY = (float) Math.toRadians(slider.getTargetValue());
              NetworkHandler.rotationChange(uuid, ModelPart.LEFT_LEG, new Rotations(
                  this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
            }));
        this.leftLegRotationZSliderButton = this.addRenderableWidget(new SliderButton(
            this.leftLegRotationYSliderButton.x + this.leftLegRotationYSliderButton.getWidth(),
            sliderTopPos, scaleWidth, 20, " leftLegRotationZ",
            (float) Math.toDegrees(leftLegRotations.getZ()), -180f, 180f, slider -> {
              this.leftLegRotationZ = (float) Math.toRadians(slider.getTargetValue());
              NetworkHandler.rotationChange(uuid, ModelPart.LEFT_LEG, new Rotations(
                  this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
            }));
        this.resetLeftLegRotationButton = this.addRenderableWidget(
            menuButton(this.leftLegRotationXSliderButton.x, sliderTopPos + 20, "reset", button -> {
              this.leftLegRotationXSliderButton.reset();
              this.leftLegRotationYSliderButton.reset();
              this.leftLegRotationZSliderButton.reset();
              this.leftLegRotationX = 0f;
              this.leftLegRotationY = 0f;
              this.leftLegRotationZ = 0f;
              NetworkHandler.rotationChange(uuid, ModelPart.LEFT_LEG, new Rotations(
                  this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
            }));
      }
    }
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderCustomPoseEntityAvatar(this.contentLeftPos + 142, this.contentTopPos + 155,
        50, this.contentLeftPos + 140 - this.xMouse, this.contentTopPos + 30 - this.yMouse,
        this.entity);

    // Rotations texts
    this.fontDraw(poseStack, "pose.head", this.headRotationXSliderButton.x + 5f,
        this.headRotationXSliderButton.y - 12f);
    this.fontDraw(poseStack, "pose.body", this.bodyRotationXSliderButton.x + 5f,
        this.bodyRotationXSliderButton.y - 12f);
    if (this.skinModel != SkinModel.VILLAGER) {
      this.fontDraw(poseStack, "pose.left_arm", this.leftArmRotationXSliderButton.x + 5f,
          this.leftArmRotationXSliderButton.y - 12f);
      this.fontDraw(poseStack, "pose.right_arm", this.rightArmRotationXSliderButton.x + 5f,
          this.rightArmRotationXSliderButton.y - 12f);
    }
    if (this.skinModel != SkinModel.ALLAY) {
      if (this.skinModel != SkinModel.FAIRY) {
        this.fontDraw(poseStack, "pose.left_leg", this.leftLegRotationXSliderButton.x + 5f,
            this.leftLegRotationXSliderButton.y - 12f);
      }
      this.fontDraw(poseStack, "pose.right_leg", this.rightLegRotationXSliderButton.x + 5f,
          this.rightLegRotationXSliderButton.y - 12f);
    }
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    // Entity background
    fill(poseStack, this.contentLeftPos + 90, this.contentTopPos, this.contentLeftPos + 191,
        this.contentTopPos + 187, 0xff000000);
    fill(poseStack, this.contentLeftPos + 91, this.contentTopPos + 1, this.contentLeftPos + 190,
        this.contentTopPos + 186, 0xffaaaaaa);
  }
}
