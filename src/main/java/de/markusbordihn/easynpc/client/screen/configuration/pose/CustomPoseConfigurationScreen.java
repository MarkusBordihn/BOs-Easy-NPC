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

import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import com.mojang.blaze3d.vertex.PoseStack;

import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.menu.configuration.pose.CustomPoseConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;

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

  public CustomPoseConfigurationScreen(CustomPoseConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  private SliderButton createVisibilityRotationPositionSlider(int left, int top,
      ModelPart modelPart, String label) {
    // Model Part Rotation
    SliderButton sliderRotationButtonX = createRotationSliderCompact(left, top, modelPart, label);

    // Model Part Position
    SliderButton sliderPositionButtonX = createPositionSliderCompact(left,
        top + sliderRotationButtonX.getHeight(), modelPart, label);

    // Model Part Visibility
    Boolean modelPartVisibility = this.entity.isModelPartVisible(modelPart);
    this.addRenderableWidget(new Checkbox(sliderRotationButtonX.getX() + 3,
        top - sliderPositionButtonX.getHeight() + 4, "", modelPartVisibility, checkbox -> {
          NetworkMessageHandler.modelVisibilityChange(uuid, modelPart, checkbox.selected());
        }));

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
      this.headSliderButton = createVisibilityRotationPositionSlider(sliderLeftPos, sliderTopPos,
          ModelPart.HEAD, "head");
    }

    // Body parts
    if (this.entity.hasBodyModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.bodySliderButton = createVisibilityRotationPositionSlider(sliderLeftPos, sliderTopPos,
          ModelPart.BODY, "body");
    }

    sliderTopPos += sliderTopSpace;

    // Arms parts
    if (!this.entity.hasLeftArmModelPart() && !this.entity.hasRightArmModelPart()
        && this.entity.hasArmsModelPart()) {
      sliderLeftPos = this.contentLeftPos - 3;
      this.armsSliderButton = createVisibilityRotationPositionSlider(sliderLeftPos, sliderTopPos,
          ModelPart.ARMS, "arms");
    }

    // Right arm parts
    if (this.entity.hasRightArmModelPart()) {
      sliderLeftPos = this.contentLeftPos - 3;
      this.rightArmSliderButton = createVisibilityRotationPositionSlider(sliderLeftPos,
          sliderTopPos, ModelPart.RIGHT_ARM, "right_arm");
    }

    // Left arm parts
    if (this.entity.hasLeftArmModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.leftArmSliderButton = createVisibilityRotationPositionSlider(sliderLeftPos, sliderTopPos,
          ModelPart.LEFT_ARM, "left_arm");
    }

    sliderTopPos += sliderTopSpace;

    // Right leg parts
    if (this.entity.hasRightLegModelPart()) {
      sliderLeftPos = this.contentLeftPos - 3;
      this.rightLegSliderButton = createVisibilityRotationPositionSlider(sliderLeftPos,
          sliderTopPos, ModelPart.RIGHT_LEG, "right_leg");
    }

    // Left leg parts
    if (this.entity.hasLeftLegModelPart()) {
      sliderLeftPos += sliderLeftSpace;
      this.leftLegSliderButton = createVisibilityRotationPositionSlider(sliderLeftPos, sliderTopPos,
          ModelPart.LEFT_LEG, "left_leg");
    }
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderCustomPoseEntityAvatar(this.contentLeftPos + 152, this.contentTopPos + 155,
        45, this.contentLeftPos + 140 - this.xMouse, this.contentTopPos + 30 - this.yMouse,
        this.entity);

    // Body parts texts
    if (this.entity.hasHeadModelPart()) {
      this.fontDraw(poseStack, "pose.head", this.headSliderButton.getX() + 20f,
          this.headSliderButton.getY() - 12f);
    }
    if (this.entity.hasBodyModelPart()) {
      this.fontDraw(poseStack, "pose.body", this.bodySliderButton.getX() + 20f,
          this.bodySliderButton.getY() - 12f);
    }
    if (this.entity.hasLeftArmModelPart()) {
      this.fontDraw(poseStack, "pose.left_arm", this.leftArmSliderButton.getX() + 20f,
          this.leftArmSliderButton.getY() - 12f);
    } else if (this.entity.hasArmsModelPart()) {
      this.fontDraw(poseStack, "pose.arms", this.armsSliderButton.getX() + 20f,
          this.armsSliderButton.getY() - 12f);
    }
    if (this.entity.hasRightArmModelPart()) {
      this.fontDraw(poseStack, "pose.right_arm", this.rightArmSliderButton.getX() + 20f,
          this.rightArmSliderButton.getY() - 12f);
    }
    if (this.entity.hasLeftLegModelPart()) {
      this.fontDraw(poseStack, "pose.left_leg", this.leftLegSliderButton.getX() + 20f,
          this.leftLegSliderButton.getY() - 12f);
    }
    if (this.entity.hasRightLegModelPart()) {
      this.fontDraw(poseStack, "pose.right_leg", this.rightLegSliderButton.getX() + 20f,
          this.rightLegSliderButton.getY() - 12f);
    }
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    // Entity background
    fill(poseStack, this.contentLeftPos + 99, this.contentTopPos, this.contentLeftPos + 206,
        this.contentTopPos + 188, 0xff000000);
    fill(poseStack, this.contentLeftPos + 100, this.contentTopPos + 1, this.contentLeftPos + 205,
        this.contentTopPos + 187, 0xffaaaaaa);
  }
}
