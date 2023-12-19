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
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.menu.configuration.pose.CustomPoseConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class CustomPoseConfigurationScreen
    extends PoseConfigurationScreen<CustomPoseConfigurationMenu> {

  // Slider Buttons reference for positioning
  protected SliderButton headSliderButton;
  protected SliderButton bodySliderButton;
  protected SliderButton armsSliderButton;
  protected SliderButton leftArmSliderButton;
  protected SliderButton rightArmSliderButton;
  protected SliderButton leftLegSliderButton;
  protected SliderButton rightLegSliderButton;

  public CustomPoseConfigurationScreen(
      CustomPoseConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private SliderButton createVisibilityRotationPositionSlider(
      int left, int top, ModelPart modelPart, String label) {
    // Model Part Rotation
    SliderButton sliderRotationButtonX = createRotationSliderCompact(left, top, modelPart, label);

    // Model Part Position
    SliderButton sliderPositionButtonX =
        createPositionSliderCompact(
            left, top + sliderRotationButtonX.getHeight(), modelPart, label);

    // Model Part Visibility
    boolean modelPartVisibility = this.entity.isModelPartVisible(modelPart);
    this.addRenderableWidget(
        new Checkbox(
            sliderRotationButtonX.x + 3,
            top - sliderPositionButtonX.getHeight(),
            "",
            modelPartVisibility,
            checkbox ->
                NetworkMessageHandler.modelVisibilityChange(uuid, modelPart, checkbox.selected())));

    return sliderRotationButtonX;
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.customPoseButton.active = false;

    // Position and size
    int sliderTopPos = this.contentTopPos + 16;
    int sliderLeftPos = this.contentLeftPos - 3;
    int sliderLeftSpace = 200;
    int sliderTopSpace = 66;

    // Head parts
    if (this.entity.hasHeadModelPart()) {
      this.headSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.HEAD, "head");
    }

    // Body parts
    if (this.entity.hasBodyModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.bodySliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.BODY, "body");
    }

    sliderTopPos += sliderTopSpace;

    // Arms parts
    if (!this.entity.hasLeftArmModelPart()
        && !this.entity.hasRightArmModelPart()
        && this.entity.hasArmsModelPart()) {
      sliderLeftPos = this.contentLeftPos - 3;
      this.armsSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.ARMS, "arms");
    }

    // Right arm parts
    if (this.entity.hasRightArmModelPart()) {
      sliderLeftPos = this.contentLeftPos - 3;
      this.rightArmSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.RIGHT_ARM, "right_arm");
    }

    // Left arm parts
    if (this.entity.hasLeftArmModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.leftArmSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.LEFT_ARM, "left_arm");
    }

    sliderTopPos += sliderTopSpace;

    // Right leg parts
    if (this.entity.hasRightLegModelPart()) {
      sliderLeftPos = this.contentLeftPos - 3;
      this.rightLegSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.RIGHT_LEG, "right_leg");
    }

    // Left leg parts
    if (this.entity.hasLeftLegModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.leftLegSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.LEFT_LEG, "left_leg");
    }
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderCustomPoseEntityAvatar(
        this.contentLeftPos + 152,
        this.contentTopPos + 155,
        45,
        this.contentLeftPos + 140 - this.xMouse,
        this.contentTopPos + 30 - this.yMouse,
        this.entity);

    // Body parts texts
    if (this.entity.hasHeadModelPart()) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.head",
          this.headSliderButton.x + 20,
          this.headSliderButton.y - 12);
    }
    if (this.entity.hasBodyModelPart()) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.body",
          this.bodySliderButton.x + 20,
          this.bodySliderButton.y - 12);
    }
    if (this.entity.hasLeftArmModelPart()) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.left_arm",
          this.leftArmSliderButton.x + 20,
          this.leftArmSliderButton.y - 12);
    } else if (this.entity.hasArmsModelPart()) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.arms",
          this.armsSliderButton.x + 20,
          this.armsSliderButton.y - 12);
    }
    if (this.entity.hasRightArmModelPart()) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.right_arm",
          this.rightArmSliderButton.x + 20,
          this.rightArmSliderButton.y - 12);
    }
    if (this.entity.hasLeftLegModelPart()) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.left_leg",
          this.leftLegSliderButton.x + 20,
          this.leftLegSliderButton.y - 12);
    }
    if (this.entity.hasRightLegModelPart()) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.right_leg",
          this.rightLegSliderButton.x + 20,
          this.rightLegSliderButton.y - 12);
    }
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    // Entity background
    fill(
        poseStack,
        this.contentLeftPos + 99,
        this.contentTopPos,
        this.contentLeftPos + 206,
        this.contentTopPos + 188,
        0xff000000);
    fill(
        poseStack,
        this.contentLeftPos + 100,
        this.contentTopPos + 1,
        this.contentLeftPos + 205,
        this.contentTopPos + 187,
        0xffaaaaaa);
  }
}
