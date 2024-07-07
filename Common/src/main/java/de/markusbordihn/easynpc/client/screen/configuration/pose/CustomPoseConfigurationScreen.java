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

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class CustomPoseConfigurationScreen<T extends ConfigurationMenu>
    extends PoseConfigurationScreen<T> {

  protected SliderButton headSliderButton;
  protected SliderButton bodySliderButton;
  protected SliderButton armsSliderButton;
  protected SliderButton leftArmSliderButton;
  protected SliderButton rightArmSliderButton;
  protected SliderButton leftLegSliderButton;
  protected SliderButton rightLegSliderButton;

  public CustomPoseConfigurationScreen(T menu, Inventory inventory, Component component) {
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
    boolean modelPartVisibility = this.modelData.isModelPartVisible(modelPart);
    this.addRenderableWidget(
        new Checkbox(
            sliderRotationButtonX.getX() + 3,
            top - sliderPositionButtonX.getHeight(),
            "",
            modelPartVisibility,
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .modelVisibilityChange(this.getNpcUUID(), modelPart, checkbox.selected())));

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

    // Variant data
    VariantData<?> variantData = this.getEasyNPC().getEasyNPCVariantData();
    boolean hasCrossedArms = variantData.hasVariantCrossedArms();

    // Head parts
    if (this.modelData.hasHeadModelPart()) {
      this.headSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.HEAD, "head");
    }

    // Body parts
    if (this.modelData.hasBodyModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.bodySliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.BODY, "body");
    }

    sliderTopPos += sliderTopSpace;

    // Arms parts
    if (hasCrossedArms
        || (!this.modelData.hasLeftArmModelPart()
            && !this.modelData.hasRightArmModelPart()
            && this.modelData.hasArmsModelPart())) {
      sliderLeftPos = this.contentLeftPos - 3;
      this.armsSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.ARMS, "arms");
    }

    // Right arm parts
    if (!hasCrossedArms && this.modelData.hasRightArmModelPart()) {
      sliderLeftPos = this.contentLeftPos - 3;
      this.rightArmSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.RIGHT_ARM, "right_arm");
    }

    // Left arm parts
    if (!hasCrossedArms && this.modelData.hasLeftArmModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.leftArmSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.LEFT_ARM, "left_arm");
    }

    sliderTopPos += sliderTopSpace;

    // Right leg parts
    if (this.modelData.hasRightLegModelPart()) {
      sliderLeftPos = this.contentLeftPos - 3;
      this.rightLegSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.RIGHT_LEG, "right_leg");
    }

    // Left leg parts
    if (this.modelData.hasLeftLegModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.leftLegSliderButton =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, ModelPart.LEFT_LEG, "left_leg");
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderCustomPoseEntityAvatar(
        this.contentLeftPos + 152,
        this.contentTopPos + 165,
        45,
        this.contentLeftPos + 150 - this.xMouse,
        this.contentTopPos + 100 - this.yMouse,
        this.getEasyNPC());

    // Body parts texts
    if (this.modelData.hasHeadModelPart() && this.headSliderButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "pose.head",
          this.headSliderButton.getX() + 20,
          this.headSliderButton.getY() - 12);
    }
    if (this.modelData.hasBodyModelPart() && this.bodySliderButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "pose.body",
          this.bodySliderButton.getX() + 20,
          this.bodySliderButton.getY() - 12);
    }
    if (this.modelData.hasLeftArmModelPart() && this.leftArmSliderButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "pose.left_arm",
          this.leftArmSliderButton.getX() + 20,
          this.leftArmSliderButton.getY() - 12);
    } else if (this.modelData.hasArmsModelPart() && this.armsSliderButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "pose.arms",
          this.armsSliderButton.getX() + 20,
          this.armsSliderButton.getY() - 12);
    }
    if (this.modelData.hasRightArmModelPart() && rightArmSliderButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "pose.right_arm",
          this.rightArmSliderButton.getX() + 20,
          this.rightArmSliderButton.getY() - 12);
    }
    if (this.modelData.hasLeftLegModelPart() && this.leftLegSliderButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "pose.left_leg",
          this.leftLegSliderButton.getX() + 20,
          this.leftLegSliderButton.getY() - 12);
    }
    if (this.modelData.hasRightLegModelPart() && this.rightLegSliderButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "pose.right_leg",
          this.rightLegSliderButton.getX() + 20,
          this.rightLegSliderButton.getY() - 12);
    }
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    // Entity background
    guiGraphics.fill(
        this.contentLeftPos + 99,
        this.contentTopPos,
        this.contentLeftPos + 206,
        this.contentTopPos + 208,
        0xff000000);
    guiGraphics.fill(
        this.contentLeftPos + 100,
        this.contentTopPos + 1,
        this.contentLeftPos + 205,
        this.contentTopPos + 207,
        0xffaaaaaa);
  }
}
