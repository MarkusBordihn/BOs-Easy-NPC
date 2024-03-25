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
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.menu.configuration.pose.AdvancedPoseConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import de.markusbordihn.easynpc.screen.ScreenHelper;
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

  private SliderButton createVisibilityRotationSlider(
      int left, int top, ModelPart modelPart, String label) {
    // Model Part Rotation
    SliderButton sliderRotationButtonX = createRotationSliderCompact(left, top, modelPart, label);

    // Model Part Visibility
    boolean modelPartVisibility = this.modelData.isModelPartVisible(modelPart);
    this.addRenderableWidget(
        new Checkbox(
            sliderRotationButtonX.x + 3,
            top - sliderRotationButtonX.getHeight(),
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
    this.advancedPoseButton.active = false;

    // Position and size
    int sliderLeftDefaultPos = this.contentLeftPos - 3;
    int sliderTopPos = this.contentTopPos + 16;
    int sliderLeftPos = sliderLeftDefaultPos;
    int sliderLeftSpace = 200;
    int sliderTopSpace = 66;

    // Variant data
    VariantData<?> variantData = this.easyNPC.getEasyNPCVariantData();
    boolean hasCrossedArms = variantData.hasVariantCrossedArms();

    // Head parts
    if (this.modelData.hasHeadModelPart()) {
      this.headRotationSliderButton =
          createVisibilityRotationSlider(sliderLeftPos, sliderTopPos, ModelPart.HEAD, "head");
    }

    // Body parts
    if (this.modelData.hasBodyModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.bodyRotationSliderButton =
          createVisibilityRotationSlider(sliderLeftPos, sliderTopPos, ModelPart.BODY, "body");
    }

    sliderTopPos += sliderTopSpace;

    // Arms parts
    if (hasCrossedArms
        || (!this.modelData.hasLeftArmModelPart()
        && !this.modelData.hasRightArmModelPart()
        && this.modelData.hasArmsModelPart())) {
      sliderLeftPos = sliderLeftDefaultPos;
      this.armsRotationSliderButton =
          createVisibilityRotationSlider(sliderLeftPos, sliderTopPos, ModelPart.ARMS, "arms");
    }

    // Right arm rotations
    if (!hasCrossedArms && this.modelData.hasRightArmModelPart()) {
      sliderLeftPos = sliderLeftDefaultPos;
      this.rightArmRotationSliderButton =
          createVisibilityRotationSlider(
              sliderLeftPos, sliderTopPos, ModelPart.RIGHT_ARM, "right_arm");
    }

    // Left arm rotations and arms rotations.
    if (!hasCrossedArms && this.modelData.hasLeftArmModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.leftArmRotationSliderButton =
          createVisibilityRotationSlider(
              sliderLeftPos, sliderTopPos, ModelPart.LEFT_ARM, "left_arm");
    }

    sliderTopPos += sliderTopSpace;
    sliderLeftPos = sliderLeftDefaultPos;

    // Right leg rotations
    if (this.modelData.hasRightLegModelPart()) {
      this.rightLegRotationSliderButton =
          createVisibilityRotationSlider(
              sliderLeftPos, sliderTopPos, ModelPart.RIGHT_LEG, "right_leg");
    }

    // Left leg rotations
    if (this.modelData.hasLeftLegModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.leftLegRotationSliderButton =
          createVisibilityRotationSlider(
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
        50,
        this.contentLeftPos + 140 - this.xMouse,
        this.contentTopPos + 30 - this.yMouse,
        this.easyNPC);

    // Body parts texts
    if (this.modelData.hasHeadModelPart() && this.headRotationSliderButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.head",
          this.headRotationSliderButton.x + 20,
          this.headRotationSliderButton.y - 12);
    }
    if (this.modelData.hasBodyModelPart() && this.bodyRotationSliderButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.body",
          this.bodyRotationSliderButton.x + 20,
          this.bodyRotationSliderButton.y - 12);
    }
    if (this.modelData.hasLeftArmModelPart() && this.leftArmRotationSliderButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.left_arm",
          this.leftArmRotationSliderButton.x + 20,
          this.leftArmRotationSliderButton.y - 12);
    } else if (this.modelData.hasArmsModelPart() && this.armsRotationSliderButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.arms",
          this.armsRotationSliderButton.x + 20,
          this.armsRotationSliderButton.y - 12);
    }
    if (this.modelData.hasRightArmModelPart() && this.rightArmRotationSliderButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.right_arm",
          this.rightArmRotationSliderButton.x + 20,
          this.rightArmRotationSliderButton.y - 12);
    }
    if (this.modelData.hasLeftLegModelPart() && this.leftLegRotationSliderButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.left_leg",
          this.leftLegRotationSliderButton.x + 20,
          this.leftLegRotationSliderButton.y - 12);
    }
    if (this.modelData.hasRightLegModelPart() && this.rightLegRotationSliderButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "pose.right_leg",
          this.rightLegRotationSliderButton.x + 20,
          this.rightLegRotationSliderButton.y - 12);
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
