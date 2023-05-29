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
import net.minecraft.network.chat.TextComponent;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import com.mojang.blaze3d.vertex.PoseStack;

import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.menu.configuration.pose.CustomPoseConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessage;

@OnlyIn(Dist.CLIENT)
public class CustomPoseConfigurationScreen
    extends PoseConfigurationScreen<CustomPoseConfigurationMenu> {

  // Internal
  protected Button resetArmsPositionButton;
  protected Button resetArmsRotationButton;
  protected Button resetBodyPositionButton;
  protected Button resetBodyRotationButton;
  protected Button resetHeadPositionButton;
  protected Button resetHeadRotationButton;
  protected Button resetLeftArmPositionButton;
  protected Button resetLeftArmRotationButton;
  protected Button resetLeftLegPositionButton;
  protected Button resetLeftLegRotationButton;
  protected Button resetRightArmPositionButton;
  protected Button resetRightArmRotationButton;
  protected Button resetRightLegPositionButton;
  protected Button resetRightLegRotationButton;
  protected Checkbox showArmsCheckbox;
  protected Checkbox showBodyCheckbox;
  protected Checkbox showHeadCheckbox;
  protected Checkbox showLeftArmCheckbox;
  protected Checkbox showLeftLegCheckbox;
  protected Checkbox showRightArmCheckbox;
  protected Checkbox showRightLegCheckbox;
  protected SliderButton armsPositionXSliderButton;
  protected SliderButton armsPositionYSliderButton;
  protected SliderButton armsPositionZSliderButton;
  protected SliderButton armsRotationXSliderButton;
  protected SliderButton armsRotationYSliderButton;
  protected SliderButton armsRotationZSliderButton;
  protected SliderButton bodyPositionXSliderButton;
  protected SliderButton bodyPositionYSliderButton;
  protected SliderButton bodyPositionZSliderButton;
  protected SliderButton bodyRotationXSliderButton;
  protected SliderButton bodyRotationYSliderButton;
  protected SliderButton bodyRotationZSliderButton;
  protected SliderButton headPositionXSliderButton;
  protected SliderButton headPositionYSliderButton;
  protected SliderButton headPositionZSliderButton;
  protected SliderButton headRotationXSliderButton;
  protected SliderButton headRotationYSliderButton;
  protected SliderButton headRotationZSliderButton;
  protected SliderButton leftArmPositionXSliderButton;
  protected SliderButton leftArmPositionYSliderButton;
  protected SliderButton leftArmPositionZSliderButton;
  protected SliderButton leftArmRotationXSliderButton;
  protected SliderButton leftArmRotationYSliderButton;
  protected SliderButton leftArmRotationZSliderButton;
  protected SliderButton leftLegPositionXSliderButton;
  protected SliderButton leftLegPositionYSliderButton;
  protected SliderButton leftLegPositionZSliderButton;
  protected SliderButton leftLegRotationXSliderButton;
  protected SliderButton leftLegRotationYSliderButton;
  protected SliderButton leftLegRotationZSliderButton;
  protected SliderButton rightArmPositionXSliderButton;
  protected SliderButton rightArmPositionYSliderButton;
  protected SliderButton rightArmPositionZSliderButton;
  protected SliderButton rightArmRotationXSliderButton;
  protected SliderButton rightArmRotationYSliderButton;
  protected SliderButton rightArmRotationZSliderButton;
  protected SliderButton rightLegPositionXSliderButton;
  protected SliderButton rightLegPositionYSliderButton;
  protected SliderButton rightLegPositionZSliderButton;
  protected SliderButton rightLegRotationXSliderButton;
  protected SliderButton rightLegRotationYSliderButton;
  protected SliderButton rightLegRotationZSliderButton;

  // Positions
  private float armsPositionX = 0f;
  private float armsPositionY = 0f;
  private float armsPositionZ = 0f;
  private float bodyPositionX = 0f;
  private float bodyPositionY = 0f;
  private float bodyPositionZ = 0f;
  private float headPositionX = 0f;
  private float headPositionY = 0f;
  private float headPositionZ = 0f;
  private float leftArmPositionX = 0f;
  private float leftArmPositionY = 0f;
  private float leftArmPositionZ = 0f;
  private float leftLegPositionX = 0f;
  private float leftLegPositionY = 0f;
  private float leftLegPositionZ = 0f;
  private float rightArmPositionX = 0f;
  private float rightArmPositionY = 0f;
  private float rightArmPositionZ = 0f;
  private float rightLegPositionX = 0f;
  private float rightLegPositionY = 0f;
  private float rightLegPositionZ = 0f;

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

  public CustomPoseConfigurationScreen(CustomPoseConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.customPoseButton.active = false;

    // Position and size
    int scaleWidth = 30;
    int sliderTopPos = this.contentTopPos + 16;
    int sliderLeftPos = this.contentLeftPos - 3;

    // Head parts
    if (this.entity.hasHeadModelPart()) {

      // Head Visible
      this.showHeadCheckbox = this.addRenderableWidget(new Checkbox(sliderLeftPos + 2,
          sliderTopPos - 16, "", this.entity.isModelHeadVisible(), checkbox -> {
            NetworkMessage.modelVisibilityChange(uuid, ModelPart.HEAD, checkbox.selected());
          }));

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
          this.headRotationXSliderButton.x + this.headRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " headRotationY",
          (float) Math.toDegrees(headRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.headRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.HEAD,
                new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
          }));
      this.headRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.headRotationYSliderButton.x + this.headRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " headRotationZ",
          (float) Math.toDegrees(headRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.headRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.HEAD,
                new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
          }));
      this.resetHeadRotationButton = this.addRenderableWidget(
          menuButton(this.headRotationZSliderButton.x + this.headRotationZSliderButton.getWidth(),
              sliderTopPos, 10, new TextComponent("↺"), button -> {
                this.headRotationXSliderButton.reset();
                this.headRotationYSliderButton.reset();
                this.headRotationZSliderButton.reset();
                this.headRotationX = 0f;
                this.headRotationY = 0f;
                this.headRotationZ = 0f;
                NetworkMessage.rotationChange(uuid, ModelPart.HEAD,
                    new Rotations(this.headRotationX, this.headRotationY, this.headRotationZ));
              }));

      // Head position
      sliderTopPos += 20;
      CustomPosition headPosition = this.entity.getModelHeadPosition();
      this.headPositionXSliderButton =
          this.addRenderableWidget(new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20,
              " headPositionX", headPosition.x(), SliderButton.Type.POSITION, slider -> {
                this.headPositionX = slider.getTargetValue();
                NetworkMessage.modelPositionChange(uuid, ModelPart.HEAD,
                    new CustomPosition(this.headPositionX, this.headPositionY, this.headPositionZ));
              }));
      this.headPositionYSliderButton = this.addRenderableWidget(new SliderButton(
          this.headPositionXSliderButton.x + this.headPositionXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " headPositionY", headPosition.y(),
          SliderButton.Type.POSITION, slider -> {
            this.headPositionY = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.HEAD,
                new CustomPosition(this.headPositionX, this.headPositionY, this.headPositionZ));
          }));
      this.headPositionZSliderButton = this.addRenderableWidget(new SliderButton(
          this.headPositionYSliderButton.x + this.headPositionYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " headPositionZ", headPosition.z(),
          SliderButton.Type.POSITION, slider -> {
            this.headPositionZ = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.HEAD,
                new CustomPosition(this.headPositionX, this.headPositionY, this.headPositionZ));
          }));
      this.resetHeadPositionButton = this.addRenderableWidget(
          menuButton(this.headPositionZSliderButton.x + this.headPositionZSliderButton.getWidth(),
              sliderTopPos, 10, new TextComponent("↺"), button -> {
                this.headPositionXSliderButton.reset();
                this.headPositionYSliderButton.reset();
                this.headPositionZSliderButton.reset();
                this.headPositionX = 0f;
                this.headPositionY = 0f;
                this.headPositionZ = 0f;
                NetworkMessage.modelPositionChange(uuid, ModelPart.HEAD,
                    new CustomPosition(this.headPositionX, this.headPositionY, this.headPositionZ));
              }));
      sliderTopPos -= 20;
    }

    // Body parts
    if (this.entity.hasBodyModelPart()) {
      sliderLeftPos += 188;

      // Body visibility
      this.showBodyCheckbox = this.addRenderableWidget(new Checkbox(sliderLeftPos + 12,
          sliderTopPos - 16, "", this.entity.isModelBodyVisible(), checkbox -> {
            NetworkMessage.modelVisibilityChange(uuid, ModelPart.BODY, checkbox.selected());
          }));

      // Body rotations
      Rotations bodyRotations = this.entity.getModelBodyRotation();
      this.resetBodyRotationButton = this.addRenderableWidget(
          menuButton(sliderLeftPos, sliderTopPos, 10, new TextComponent("↺"), button -> {
            this.bodyRotationXSliderButton.reset();
            this.bodyRotationYSliderButton.reset();
            this.bodyRotationZSliderButton.reset();
            this.bodyRotationX = 0f;
            this.bodyRotationY = 0f;
            this.bodyRotationZ = 0f;
            NetworkMessage.rotationChange(uuid, ModelPart.BODY,
                new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
          }));
      this.bodyRotationXSliderButton = this.addRenderableWidget(
          new SliderButton(this.resetBodyRotationButton.x + this.resetBodyRotationButton.getWidth(),
              sliderTopPos, scaleWidth, 20, " bodyRotationX",
              (float) Math.toDegrees(bodyRotations.getX()), SliderButton.Type.DEGREE, slider -> {
                this.bodyRotationX = (float) Math.toRadians(slider.getTargetValue());
                NetworkMessage.rotationChange(uuid, ModelPart.BODY,
                    new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
              }));
      this.bodyRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.bodyRotationXSliderButton.x + this.bodyRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " bodyRotationY",
          (float) Math.toDegrees(bodyRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.bodyRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.BODY,
                new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
          }));
      this.bodyRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.bodyRotationYSliderButton.x + this.bodyRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " bodyRotationZ",
          (float) Math.toDegrees(bodyRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.bodyRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.BODY,
                new Rotations(this.bodyRotationX, this.bodyRotationY, this.bodyRotationZ));
          }));

      // Body position
      sliderTopPos += 20;
      CustomPosition bodyPosition = this.entity.getModelBodyPosition();
      this.resetBodyPositionButton = this.addRenderableWidget(
          menuButton(sliderLeftPos, sliderTopPos, 10, new TextComponent("↺"), button -> {
            this.bodyPositionXSliderButton.reset();
            this.bodyPositionYSliderButton.reset();
            this.bodyPositionZSliderButton.reset();
            this.bodyPositionX = 0f;
            this.bodyPositionY = 0f;
            this.bodyPositionZ = 0f;
            NetworkMessage.modelPositionChange(uuid, ModelPart.BODY,
                new CustomPosition(this.bodyPositionX, this.bodyPositionY, this.bodyPositionZ));
          }));
      this.bodyPositionXSliderButton = this.addRenderableWidget(
          new SliderButton(this.resetBodyPositionButton.x + this.resetBodyPositionButton.getWidth(),
              sliderTopPos, scaleWidth, 20, " bodyPositionX", bodyPosition.x(),
              SliderButton.Type.POSITION, slider -> {
                this.bodyPositionX = slider.getTargetValue();
                NetworkMessage.modelPositionChange(uuid, ModelPart.BODY,
                    new CustomPosition(this.bodyPositionX, this.bodyPositionY, this.bodyPositionZ));
              }));
      this.bodyPositionYSliderButton = this.addRenderableWidget(new SliderButton(
          this.bodyPositionXSliderButton.x + this.bodyPositionXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " bodyPositionY", bodyPosition.y(),
          SliderButton.Type.POSITION, slider -> {
            this.bodyPositionY = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.BODY,
                new CustomPosition(this.bodyPositionX, this.bodyPositionY, this.bodyPositionZ));
          }));
      this.bodyPositionZSliderButton = this.addRenderableWidget(new SliderButton(
          this.bodyPositionYSliderButton.x + this.bodyPositionYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " bodyPositionZ", bodyPosition.z(),
          SliderButton.Type.POSITION, slider -> {
            this.bodyPositionZ = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.BODY,
                new CustomPosition(this.bodyPositionX, this.bodyPositionY, this.bodyPositionZ));
          }));
      sliderTopPos -= 20;
    }

    sliderTopPos += 66;

    // Arms parts
    if (!this.entity.hasLeftArmModelPart() && !this.entity.hasRightArmModelPart()
        && this.entity.hasArmsModelPart()) {
      sliderLeftPos = this.contentLeftPos - 3;

      // Arms visibility
      this.showArmsCheckbox = this.addRenderableWidget(new Checkbox(sliderLeftPos + 2,
          sliderTopPos - 16, "", this.entity.isModelArmsVisible(), checkbox -> {
            NetworkMessage.modelVisibilityChange(uuid, ModelPart.ARMS, checkbox.selected());
          }));

      // Arms rotation
      Rotations armsRotations = this.entity.getModelArmsRotation();
      this.armsRotationXSliderButton = this.addRenderableWidget(
          new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20, " armsRotationX",
              (float) Math.toDegrees(armsRotations.getX()), SliderButton.Type.DEGREE, slider -> {
                this.armsRotationX = (float) Math.toRadians(slider.getTargetValue());
                NetworkMessage.rotationChange(uuid, ModelPart.ARMS,
                    new Rotations(this.armsRotationX, this.armsRotationY, this.armsRotationZ));
              }));
      this.armsRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.armsRotationXSliderButton.x + this.armsRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " armsRotationY",
          (float) Math.toDegrees(armsRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.armsRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.ARMS,
                new Rotations(this.armsRotationX, this.armsRotationY, this.armsRotationZ));
          }));
      this.armsRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.armsRotationYSliderButton.x + this.armsRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " armsRotationZ",
          (float) Math.toDegrees(armsRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.armsRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.ARMS,
                new Rotations(this.armsRotationX, this.armsRotationY, this.armsRotationZ));
          }));
      this.resetArmsRotationButton = this.addRenderableWidget(
          menuButton(this.armsRotationZSliderButton.x + this.armsRotationZSliderButton.getWidth(),
              sliderTopPos, 10, new TextComponent("↺"), button -> {
                this.armsRotationXSliderButton.reset();
                this.armsRotationYSliderButton.reset();
                this.armsRotationZSliderButton.reset();
                this.armsRotationX = 0f;
                this.armsRotationY = 0f;
                this.armsRotationZ = 0f;
                NetworkMessage.rotationChange(uuid, ModelPart.ARMS,
                    new Rotations(this.armsRotationX, this.armsRotationY, this.armsRotationZ));
              }));

      // Arms position
      sliderTopPos += 20;
      CustomPosition armsPosition = this.entity.getModelArmsPosition();
      this.armsPositionXSliderButton =
          this.addRenderableWidget(new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20,
              " armsPositionX", armsPosition.x(), SliderButton.Type.POSITION, slider -> {
                this.armsPositionX = slider.getTargetValue();
                NetworkMessage.modelPositionChange(uuid, ModelPart.ARMS,
                    new CustomPosition(this.armsPositionX, this.armsPositionY, this.armsPositionZ));
              }));
      this.armsPositionYSliderButton = this.addRenderableWidget(new SliderButton(
          this.armsPositionXSliderButton.x + this.armsPositionXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " armsPositionY", armsPosition.y(),
          SliderButton.Type.POSITION, slider -> {
            this.armsPositionY = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.ARMS,
                new CustomPosition(this.armsPositionX, this.armsPositionY, this.armsPositionZ));
          }));
      this.armsPositionZSliderButton = this.addRenderableWidget(new SliderButton(
          this.armsPositionYSliderButton.x + this.armsPositionYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " armsPositionZ", armsPosition.z(),
          SliderButton.Type.POSITION, slider -> {
            this.armsPositionZ = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.ARMS,
                new CustomPosition(this.armsPositionX, this.armsPositionY, this.armsPositionZ));
          }));
      this.resetArmsPositionButton = this.addRenderableWidget(
          menuButton(this.armsPositionZSliderButton.x + this.armsPositionZSliderButton.getWidth(),
              sliderTopPos, 10, new TextComponent("↺"), button -> {
                this.armsPositionXSliderButton.reset();
                this.armsPositionYSliderButton.reset();
                this.armsPositionZSliderButton.reset();
                this.armsPositionX = 0f;
                this.armsPositionY = 0f;
                this.armsPositionZ = 0f;
                NetworkMessage.modelPositionChange(uuid, ModelPart.ARMS,
                    new CustomPosition(this.armsPositionX, this.armsPositionY, this.armsPositionZ));
              }));
      sliderTopPos -= 20;
    }

    // Right arm parts
    if (this.entity.hasRightArmModelPart()) {
      sliderLeftPos = this.contentLeftPos - 3;

      // Right arm visibility
      this.showRightArmCheckbox = this.addRenderableWidget(new Checkbox(sliderLeftPos + 2,
          sliderTopPos - 16, "", this.entity.isModelRightArmVisible(), checkbox -> {
            NetworkMessage.modelVisibilityChange(uuid, ModelPart.RIGHT_ARM, checkbox.selected());
          }));

      // Right arm rotations
      Rotations rightArmRotations = this.entity.getModelRightArmRotation();
      this.rightArmRotationXSliderButton = this.addRenderableWidget(new SliderButton(sliderLeftPos,
          sliderTopPos, scaleWidth, 20, " rightArmRotationX",
          (float) Math.toDegrees(rightArmRotations.getX()), SliderButton.Type.DEGREE, slider -> {
            this.rightArmRotationX = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
          }));
      this.rightArmRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightArmRotationXSliderButton.x + this.rightArmRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightArmRotationY",
          (float) Math.toDegrees(rightArmRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.rightArmRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
          }));
      this.rightArmRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightArmRotationYSliderButton.x + this.rightArmRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightArmRotationZ",
          (float) Math.toDegrees(rightArmRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.rightArmRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
          }));
      this.resetRightArmRotationButton = this.addRenderableWidget(menuButton(
          this.rightArmRotationZSliderButton.x + this.rightArmRotationZSliderButton.getWidth(),
          sliderTopPos, 10, new TextComponent("↺"), button -> {
            this.rightArmRotationXSliderButton.reset();
            this.rightArmRotationYSliderButton.reset();
            this.rightArmRotationZSliderButton.reset();
            this.rightArmRotationX = 0f;
            this.rightArmRotationY = 0f;
            this.rightArmRotationZ = 0f;
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_ARM, new Rotations(
                this.rightArmRotationX, this.rightArmRotationY, this.rightArmRotationZ));
          }));

      // Right arm position
      sliderTopPos += 20;
      CustomPosition rightArmPosition = this.entity.getModelRightArmPosition();
      this.rightArmPositionXSliderButton =
          this.addRenderableWidget(new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20,
              " rightArmPositionX", rightArmPosition.x(), SliderButton.Type.POSITION, slider -> {
                this.rightArmPositionX = slider.getTargetValue();
                NetworkMessage.modelPositionChange(uuid, ModelPart.RIGHT_ARM, new CustomPosition(
                    this.rightArmPositionX, this.rightArmPositionY, this.rightArmPositionZ));
              }));
      this.rightArmPositionYSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightArmPositionXSliderButton.x + this.rightArmPositionXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightArmPositionY", rightArmPosition.y(),
          SliderButton.Type.POSITION, slider -> {
            this.rightArmPositionY = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.RIGHT_ARM, new CustomPosition(
                this.rightArmPositionX, this.rightArmPositionY, this.rightArmPositionZ));
          }));
      this.rightArmPositionZSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightArmPositionYSliderButton.x + this.rightArmPositionYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightArmPositionZ", rightArmPosition.z(),
          SliderButton.Type.POSITION, slider -> {
            this.rightArmPositionZ = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.RIGHT_ARM, new CustomPosition(
                this.rightArmPositionX, this.rightArmPositionY, this.rightArmPositionZ));
          }));
      this.resetRightArmPositionButton = this.addRenderableWidget(menuButton(
          this.rightArmPositionZSliderButton.x + this.rightArmPositionZSliderButton.getWidth(),
          sliderTopPos, 10, new TextComponent("↺"), button -> {
            this.rightArmPositionXSliderButton.reset();
            this.rightArmPositionYSliderButton.reset();
            this.rightArmPositionZSliderButton.reset();
            this.rightArmPositionX = 0f;
            this.rightArmPositionY = 0f;
            this.rightArmPositionZ = 0f;
            NetworkMessage.modelPositionChange(uuid, ModelPart.RIGHT_ARM, new CustomPosition(
                this.rightArmPositionX, this.rightArmPositionY, this.rightArmPositionZ));
          }));
      sliderTopPos -= 20;
    }

    // Left arm parts
    if (this.entity.hasLeftArmModelPart()) {
      sliderLeftPos += 188;

      // Left arm visibility
      this.showLeftArmCheckbox = this.addRenderableWidget(new Checkbox(sliderLeftPos + 12,
          sliderTopPos - 16, "", this.entity.isModelLeftArmVisible(), checkbox -> {
            NetworkMessage.modelVisibilityChange(uuid, ModelPart.LEFT_ARM, checkbox.selected());
          }));

      // Left arm rotation
      Rotations leftArmRotations = this.entity.getModelLeftArmRotation();
      this.resetLeftArmRotationButton = this.addRenderableWidget(
          menuButton(sliderLeftPos, sliderTopPos, 10, new TextComponent("↺"), button -> {
            this.leftArmRotationXSliderButton.reset();
            this.leftArmRotationYSliderButton.reset();
            this.leftArmRotationZSliderButton.reset();
            this.leftArmRotationX = 0f;
            this.leftArmRotationY = 0f;
            this.leftArmRotationZ = 0f;
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_ARM,
                new Rotations(this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
          }));
      this.leftArmRotationXSliderButton = this.addRenderableWidget(new SliderButton(
          this.resetLeftArmRotationButton.x + this.resetLeftArmRotationButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftArmRotationX",
          (float) Math.toDegrees(leftArmRotations.getX()), SliderButton.Type.DEGREE, slider -> {
            this.leftArmRotationX = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_ARM,
                new Rotations(this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
          }));
      this.leftArmRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftArmRotationXSliderButton.x + this.leftArmRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftArmRotationY",
          (float) Math.toDegrees(leftArmRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.leftArmRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_ARM,
                new Rotations(this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
          }));
      this.leftArmRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftArmRotationYSliderButton.x + this.leftArmRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftArmRotationZ",
          (float) Math.toDegrees(leftArmRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.leftArmRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_ARM,
                new Rotations(this.leftArmRotationX, this.leftArmRotationY, this.leftArmRotationZ));
          }));

      // Left arm position
      sliderTopPos += 20;
      CustomPosition leftArmPosition = this.entity.getModelLeftArmPosition();
      this.resetLeftArmPositionButton = this.addRenderableWidget(
          menuButton(sliderLeftPos, sliderTopPos, 10, new TextComponent("↺"), button -> {
            this.leftArmPositionXSliderButton.reset();
            this.leftArmPositionYSliderButton.reset();
            this.leftArmPositionZSliderButton.reset();
            this.leftArmPositionX = 0f;
            this.leftArmPositionY = 0f;
            this.leftArmPositionZ = 0f;
            NetworkMessage.modelPositionChange(uuid, ModelPart.LEFT_ARM, new CustomPosition(
                this.leftArmPositionX, this.leftArmPositionY, this.leftArmPositionZ));
          }));
      this.leftArmPositionXSliderButton = this.addRenderableWidget(new SliderButton(
          this.resetLeftArmPositionButton.x + this.resetLeftArmPositionButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftArmPositionX", leftArmPosition.x(),
          SliderButton.Type.POSITION, slider -> {
            this.leftArmPositionX = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.LEFT_ARM, new CustomPosition(
                this.leftArmPositionX, this.leftArmPositionY, this.leftArmPositionZ));
          }));
      this.leftArmPositionYSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftArmPositionXSliderButton.x + this.leftArmPositionXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftArmPositionY", leftArmPosition.y(),
          SliderButton.Type.POSITION, slider -> {
            this.leftArmPositionY = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.LEFT_ARM, new CustomPosition(
                this.leftArmPositionX, this.leftArmPositionY, this.leftArmPositionZ));
          }));
      this.leftArmPositionZSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftArmPositionYSliderButton.x + this.leftArmPositionYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftArmPositionZ", leftArmPosition.z(),
          SliderButton.Type.POSITION, slider -> {
            this.leftArmPositionZ = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.LEFT_ARM, new CustomPosition(
                this.leftArmPositionX, this.leftArmPositionY, this.leftArmPositionZ));
          }));
      sliderTopPos -= 20;
    }

    sliderTopPos += 66;
    sliderLeftPos = this.contentLeftPos - 3;

    // Right leg parts
    if (this.entity.hasRightLegModelPart()) {

      // Right leg visibility
      this.showRightLegCheckbox = this.addRenderableWidget(new Checkbox(sliderLeftPos + 2,
          sliderTopPos - 16, "", this.entity.isModelRightLegVisible(), checkbox -> {
            NetworkMessage.modelVisibilityChange(uuid, ModelPart.RIGHT_LEG, checkbox.selected());
          }));

      // Right leg rotation
      Rotations rightLegRotations = this.entity.getModelRightLegRotation();
      this.rightLegRotationXSliderButton = this.addRenderableWidget(new SliderButton(sliderLeftPos,
          sliderTopPos, scaleWidth, 20, " rightLegRotationX",
          (float) Math.toDegrees(rightLegRotations.getX()), SliderButton.Type.DEGREE, slider -> {
            this.rightLegRotationX = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
          }));
      this.rightLegRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightLegRotationXSliderButton.x + this.rightLegRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightLegRotationY",
          (float) Math.toDegrees(rightLegRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.rightLegRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
          }));
      this.rightLegRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightLegRotationYSliderButton.x + this.rightLegRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightLegRotationZ",
          (float) Math.toDegrees(rightLegRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.rightLegRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
          }));
      this.resetRightLegRotationButton = this.addRenderableWidget(menuButton(
          this.rightLegRotationZSliderButton.x + this.rightLegRotationZSliderButton.getWidth(),
          sliderTopPos, 10, new TextComponent("↺"), button -> {
            this.rightLegRotationXSliderButton.reset();
            this.rightLegRotationYSliderButton.reset();
            this.rightLegRotationZSliderButton.reset();
            this.rightLegRotationX = 0f;
            this.rightLegRotationY = 0f;
            this.rightLegRotationZ = 0f;
            NetworkMessage.rotationChange(uuid, ModelPart.RIGHT_LEG, new Rotations(
                this.rightLegRotationX, this.rightLegRotationY, this.rightLegRotationZ));
          }));

      // Right leg position
      sliderTopPos += 20;
      CustomPosition rightLegPosition = this.entity.getModelRightLegPosition();
      this.rightLegPositionXSliderButton =
          this.addRenderableWidget(new SliderButton(sliderLeftPos, sliderTopPos, scaleWidth, 20,
              " rightLegPositionX", rightLegPosition.x(), SliderButton.Type.POSITION, slider -> {
                this.rightLegPositionX = slider.getTargetValue();
                NetworkMessage.modelPositionChange(uuid, ModelPart.RIGHT_LEG, new CustomPosition(
                    this.rightLegPositionX, this.rightLegPositionY, this.rightLegPositionZ));
              }));
      this.rightLegPositionYSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightLegPositionXSliderButton.x + this.rightLegPositionXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightLegPositionY", rightLegPosition.y(),
          SliderButton.Type.POSITION, slider -> {
            this.rightLegPositionY = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.RIGHT_LEG, new CustomPosition(
                this.rightLegPositionX, this.rightLegPositionY, this.rightLegPositionZ));
          }));
      this.rightLegPositionZSliderButton = this.addRenderableWidget(new SliderButton(
          this.rightLegPositionYSliderButton.x + this.rightLegPositionYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " rightLegPositionZ", rightLegPosition.z(),
          SliderButton.Type.POSITION, slider -> {
            this.rightLegPositionZ = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.RIGHT_LEG, new CustomPosition(
                this.rightLegPositionX, this.rightLegPositionY, this.rightLegPositionZ));
          }));
      this.resetRightLegPositionButton = this.addRenderableWidget(menuButton(
          this.rightLegPositionZSliderButton.x + this.rightLegPositionZSliderButton.getWidth(),
          sliderTopPos, 10, new TextComponent("↺"), button -> {
            this.rightLegPositionXSliderButton.reset();
            this.rightLegPositionYSliderButton.reset();
            this.rightLegPositionZSliderButton.reset();
            this.rightLegPositionX = 0f;
            this.rightLegPositionY = 0f;
            this.rightLegPositionZ = 0f;
            NetworkMessage.modelPositionChange(uuid, ModelPart.RIGHT_LEG, new CustomPosition(
                this.rightLegPositionX, this.rightLegPositionY, this.rightLegPositionZ));
          }));
      sliderTopPos -= 20;
    }

    // Left leg parts
    if (this.entity.hasLeftLegModelPart()) {
      sliderLeftPos += 188;

      // Left leg visibility
      this.showLeftLegCheckbox = this.addRenderableWidget(new Checkbox(sliderLeftPos + 12,
          sliderTopPos - 16, "", this.entity.isModelLeftLegVisible(), checkbox -> {
            NetworkMessage.modelVisibilityChange(uuid, ModelPart.LEFT_LEG, checkbox.selected());
          }));

      // Left leg rotations
      Rotations leftLegRotations = this.entity.getModelLeftLegRotation();
      this.resetLeftLegRotationButton = this.addRenderableWidget(
          menuButton(sliderLeftPos, sliderTopPos, 10, new TextComponent("↺"), button -> {
            this.leftLegRotationXSliderButton.reset();
            this.leftLegRotationYSliderButton.reset();
            this.leftLegRotationZSliderButton.reset();
            this.leftLegRotationX = 0f;
            this.leftLegRotationY = 0f;
            this.leftLegRotationZ = 0f;
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_LEG,
                new Rotations(this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
          }));
      this.leftLegRotationXSliderButton = this.addRenderableWidget(new SliderButton(
          this.resetLeftLegRotationButton.x + this.resetLeftLegRotationButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftLegRotationX",
          (float) Math.toDegrees(leftLegRotations.getX()), SliderButton.Type.DEGREE, slider -> {
            this.leftLegRotationX = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_LEG,
                new Rotations(this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
          }));
      this.leftLegRotationYSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftLegRotationXSliderButton.x + this.leftLegRotationXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftLegRotationY",
          (float) Math.toDegrees(leftLegRotations.getY()), SliderButton.Type.DEGREE, slider -> {
            this.leftLegRotationY = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_LEG,
                new Rotations(this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
          }));
      this.leftLegRotationZSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftLegRotationYSliderButton.x + this.leftLegRotationYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftLegRotationZ",
          (float) Math.toDegrees(leftLegRotations.getZ()), SliderButton.Type.DEGREE, slider -> {
            this.leftLegRotationZ = (float) Math.toRadians(slider.getTargetValue());
            NetworkMessage.rotationChange(uuid, ModelPart.LEFT_LEG,
                new Rotations(this.leftLegRotationX, this.leftLegRotationY, this.leftLegRotationZ));
          }));

      // Left leg position
      sliderTopPos += 20;
      CustomPosition leftLegPosition = this.entity.getModelLeftLegPosition();
      this.resetLeftLegPositionButton = this.addRenderableWidget(
          menuButton(sliderLeftPos, sliderTopPos, 10, new TextComponent("↺"), button -> {
            this.leftLegPositionXSliderButton.reset();
            this.leftLegPositionYSliderButton.reset();
            this.leftLegPositionZSliderButton.reset();
            this.leftLegPositionX = 0f;
            this.leftLegPositionY = 0f;
            this.leftLegPositionZ = 0f;
            NetworkMessage.modelPositionChange(uuid, ModelPart.LEFT_LEG, new CustomPosition(
                this.leftLegPositionX, this.leftLegPositionY, this.leftLegPositionZ));
          }));
      this.leftLegPositionXSliderButton = this.addRenderableWidget(new SliderButton(
          this.resetLeftLegRotationButton.x + this.resetLeftLegRotationButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftLegPositionX", leftLegPosition.x(),
          SliderButton.Type.POSITION, slider -> {
            this.leftLegPositionX = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.LEFT_LEG, new CustomPosition(
                this.leftLegPositionX, this.leftLegPositionY, this.leftLegPositionZ));
          }));
      this.leftLegPositionYSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftLegPositionXSliderButton.x + this.leftLegPositionXSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftLegPositionY", leftLegPosition.y(),
          SliderButton.Type.POSITION, slider -> {
            this.leftLegPositionY = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.LEFT_LEG, new CustomPosition(
                this.leftLegPositionX, this.leftLegPositionY, this.leftLegPositionZ));
          }));
      this.leftLegPositionZSliderButton = this.addRenderableWidget(new SliderButton(
          this.leftLegPositionYSliderButton.x + this.leftLegPositionYSliderButton.getWidth(),
          sliderTopPos, scaleWidth, 20, " leftLegPositionZ", leftLegPosition.z(),
          SliderButton.Type.POSITION, slider -> {
            this.leftLegPositionZ = slider.getTargetValue();
            NetworkMessage.modelPositionChange(uuid, ModelPart.LEFT_LEG, new CustomPosition(
                this.leftLegPositionX, this.leftLegPositionY, this.leftLegPositionZ));
          }));
      sliderTopPos -= 20;
    }
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderCustomPoseEntityAvatar(this.contentLeftPos + 142, this.contentTopPos + 155,
        50, this.contentLeftPos + 140 - this.xMouse, this.contentTopPos + 30 - this.yMouse,
        this.entity);

    // Body parts texts
    if (this.entity.hasHeadModelPart()) {
      this.fontDraw(poseStack, "pose.head", this.headRotationXSliderButton.x + 20f,
          this.headRotationXSliderButton.y - 12f);
    }
    if (this.entity.hasBodyModelPart()) {
      this.fontDraw(poseStack, "pose.body", this.bodyRotationXSliderButton.x + 20f,
          this.bodyRotationXSliderButton.y - 12f);
    }
    if (this.entity.hasLeftArmModelPart()) {
      this.fontDraw(poseStack, "pose.left_arm", this.leftArmRotationXSliderButton.x + 20f,
          this.leftArmRotationXSliderButton.y - 12f);
    } else if (this.entity.hasArmsModelPart()) {
      this.fontDraw(poseStack, "pose.arms", this.armsRotationXSliderButton.x + 20f,
          this.armsRotationXSliderButton.y - 12f);
    }
    if (this.entity.hasRightArmModelPart()) {
      this.fontDraw(poseStack, "pose.right_arm", this.rightArmRotationXSliderButton.x + 20f,
          this.rightArmRotationXSliderButton.y - 12f);
    }
    if (this.entity.hasLeftLegModelPart()) {
      this.fontDraw(poseStack, "pose.left_leg", this.leftLegRotationXSliderButton.x + 20f,
          this.leftLegRotationXSliderButton.y - 12f);
    }
    if (this.entity.hasRightLegModelPart()) {
      this.fontDraw(poseStack, "pose.right_leg", this.rightLegRotationXSliderButton.x + 20f,
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
