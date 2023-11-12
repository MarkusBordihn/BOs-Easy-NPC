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

package de.markusbordihn.easynpc.client.screen.configuration.pose;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.menu.configuration.pose.AdvancedPoseConfigurationMenu;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class AdvancedPoseConfigurationScreen
    extends PoseConfigurationScreen<AdvancedPoseConfigurationMenu> {

  // Slider Buttons reference for positioning
  protected SliderButton headRotationSliderButton;
  protected SliderButton bodyRotationSliderButton;
  protected SliderButton armsRotationSliderButton;
  protected SliderButton leftArmRotationSliderButton;
  protected SliderButton rightArmRotationSliderButton;
  protected SliderButton leftLegRotationSliderButton;
  protected SliderButton rightLegRotationSliderButton;

  public AdvancedPoseConfigurationScreen(
      AdvancedPoseConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.advancedPoseButton.active = false;

    // Position and size
    int sliderTopPos = this.contentTopPos + 16;
    int sliderLeftPos = this.contentLeftPos;
    int sliderLeftSpace = 200;
    int sliderTopSpace = 66;

    // Head parts
    if (this.entity.hasHeadModelPart()) {
      this.headRotationSliderButton =
          createRotationSlider(sliderLeftPos, sliderTopPos, ModelPart.HEAD, "head");
    }

    // Body parts
    if (this.entity.hasBodyModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.bodyRotationSliderButton =
          createRotationSlider(sliderLeftPos, sliderTopPos, ModelPart.BODY, "body");
    }

    sliderTopPos += sliderTopSpace;

    // Right arm rotations
    if (this.entity.hasRightArmModelPart()) {
      sliderLeftPos = this.contentLeftPos;
      this.rightArmRotationSliderButton =
          createRotationSlider(sliderLeftPos, sliderTopPos, ModelPart.RIGHT_ARM, "right_arm");
    }

    // Left arm rotations and arms rotations.
    if (this.entity.hasLeftArmModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.leftArmRotationSliderButton =
          createRotationSlider(sliderLeftPos, sliderTopPos, ModelPart.LEFT_ARM, "left_arm");
    } else if (this.entity.hasArmsModelPart()) {
      sliderLeftPos = this.contentLeftPos;
      this.armsRotationSliderButton =
          createRotationSlider(sliderLeftPos, sliderTopPos, ModelPart.ARMS, "arms");
    }

    sliderTopPos += sliderTopSpace;
    sliderLeftPos = this.contentLeftPos;

    // Right leg rotations
    if (this.entity.hasRightLegModelPart()) {
      this.rightLegRotationSliderButton =
          createRotationSlider(sliderLeftPos, sliderTopPos, ModelPart.RIGHT_LEG, "right_leg");
    }

    // Left leg rotations
    if (this.entity.hasLeftLegModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.leftLegRotationSliderButton =
          createRotationSlider(sliderLeftPos, sliderTopPos, ModelPart.LEFT_LEG, "left_leg");
    }
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderCustomPoseEntityAvatar(
        this.contentLeftPos + 152,
        this.contentTopPos + 155,
        50,
        this.contentLeftPos + 140 - this.xMouse,
        this.contentTopPos + 30 - this.yMouse,
        this.entity);

    // Body parts texts
    if (this.entity.hasHeadModelPart()) {
      this.fontDraw(
          poseStack,
          "pose.head",
          this.headRotationSliderButton.x + 5,
          this.headRotationSliderButton.y - 12);
    }
    if (this.entity.hasBodyModelPart()) {
      this.fontDraw(
          poseStack,
          "pose.body",
          this.bodyRotationSliderButton.x + 5,
          this.bodyRotationSliderButton.y - 12);
    }
    if (this.entity.hasLeftArmModelPart()) {
      this.fontDraw(
          poseStack,
          "pose.left_arm",
          this.leftArmRotationSliderButton.x + 5,
          this.leftArmRotationSliderButton.y - 12);
    } else if (this.entity.hasArmsModelPart()) {
      this.fontDraw(
          poseStack,
          "pose.arms",
          this.armsRotationSliderButton.x + 5,
          this.armsRotationSliderButton.y - 12);
    }
    if (this.entity.hasRightArmModelPart()) {
      this.fontDraw(
          poseStack,
          "pose.right_arm",
          this.rightArmRotationSliderButton.x + 5,
          this.rightArmRotationSliderButton.y - 12);
    }
    if (this.entity.hasLeftLegModelPart()) {
      this.fontDraw(
          poseStack,
          "pose.left_leg",
          this.leftLegRotationSliderButton.x + 5,
          this.leftLegRotationSliderButton.y - 12);
    }
    if (this.entity.hasRightLegModelPart()) {
      this.fontDraw(
          poseStack,
          "pose.right_leg",
          this.rightLegRotationSliderButton.x + 5,
          this.rightLegRotationSliderButton.y - 12);
    }
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    // Entity background
    fill(
        poseStack,
        this.contentLeftPos + 101,
        this.contentTopPos,
        this.contentLeftPos + 201,
        this.contentTopPos + 188,
        0xff000000);
    fill(
        poseStack,
        this.contentLeftPos + 102,
        this.contentTopPos + 1,
        this.contentLeftPos + 200,
        this.contentTopPos + 187,
        0xffaaaaaa);
  }
}
