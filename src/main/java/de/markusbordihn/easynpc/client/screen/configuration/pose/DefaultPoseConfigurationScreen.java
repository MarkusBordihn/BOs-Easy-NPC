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
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import com.mojang.blaze3d.vertex.PoseStack;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.menu.configuration.pose.DefaultPoseConfigurationMenu;
import de.markusbordihn.easynpc.model.ModelPose;
import de.markusbordihn.easynpc.network.NetworkHandler;

@OnlyIn(Dist.CLIENT)
public class DefaultPoseConfigurationScreen
    extends PoseConfigurationScreen<DefaultPoseConfigurationMenu> {

  // Pose buttons
  protected Button crouchingPoseButton;
  protected Button dyingPoseButton;
  protected Button fallFlyingPoseButton;
  protected Button longJumpPoseButton;
  protected Button sleepingPoseButton;
  protected Button spinAttackPoseButton;
  protected Button standingPoseButton;
  protected Button swimmingPoseButton;

  public DefaultPoseConfigurationScreen(DefaultPoseConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  private Button menuButton(int left, int top, String label, Button.OnPress onPress) {
    return Button
        .builder(Component.translatable(Constants.TEXT_CONFIG_PREFIX + "pose." + label), onPress)
        .bounds(left, top, 100, 20).build();
  }

  private void checkPoseButtonState(Pose pose, ModelPose modelPose) {
    Pose currentPose = pose != null ? pose : this.entity.getPose();
    boolean isCustomModelPose =
        (modelPose != null ? modelPose : this.entity.getModelPose()) == ModelPose.CUSTOM;
    this.standingPoseButton.active = isCustomModelPose || currentPose != Pose.STANDING;
    this.crouchingPoseButton.active = isCustomModelPose || currentPose != Pose.CROUCHING;
    this.dyingPoseButton.active = isCustomModelPose || currentPose != Pose.DYING;
    this.fallFlyingPoseButton.active = isCustomModelPose || currentPose != Pose.FALL_FLYING;
    this.longJumpPoseButton.active = isCustomModelPose || currentPose != Pose.LONG_JUMPING;
    this.sleepingPoseButton.active = isCustomModelPose || currentPose != Pose.SLEEPING;
    this.spinAttackPoseButton.active = isCustomModelPose || currentPose != Pose.SPIN_ATTACK;
    this.swimmingPoseButton.active = isCustomModelPose || currentPose != Pose.SWIMMING;
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.defaultPoseButton.active = false;
    this.customPoseButton.active = true;

    // Pose Buttons
    int poseButtonLeft = this.contentLeftPos + 175;
    this.standingPoseButton = this
        .addRenderableWidget(menuButton(poseButtonLeft, this.contentTopPos, "standing", button -> {
          NetworkHandler.poseChange(uuid, Pose.STANDING);
          this.checkPoseButtonState(Pose.STANDING, ModelPose.DEFAULT);
        }));
    this.crouchingPoseButton = this.addRenderableWidget(
        menuButton(poseButtonLeft, this.contentTopPos + 24, "crouching", button -> {
          NetworkHandler.poseChange(uuid, Pose.CROUCHING);
          this.checkPoseButtonState(Pose.CROUCHING, ModelPose.DEFAULT);
        }));
    this.dyingPoseButton = this.addRenderableWidget(
        menuButton(poseButtonLeft, this.contentTopPos + 48, "dying", button -> {
          NetworkHandler.poseChange(uuid, Pose.DYING);
          this.checkPoseButtonState(Pose.DYING, ModelPose.DEFAULT);
        }));
    this.fallFlyingPoseButton = this.addRenderableWidget(
        menuButton(poseButtonLeft, this.contentTopPos + 72, "fall_flying", button -> {
          NetworkHandler.poseChange(uuid, Pose.FALL_FLYING);
          this.checkPoseButtonState(Pose.FALL_FLYING, ModelPose.DEFAULT);
        }));
    this.longJumpPoseButton = this.addRenderableWidget(
        menuButton(poseButtonLeft, this.contentTopPos + 96, "long_jumping", button -> {
          NetworkHandler.poseChange(uuid, Pose.LONG_JUMPING);
          this.checkPoseButtonState(Pose.LONG_JUMPING, ModelPose.DEFAULT);
        }));
    this.sleepingPoseButton = this.addRenderableWidget(
        menuButton(poseButtonLeft, this.contentTopPos + 120, "sleeping", button -> {
          NetworkHandler.poseChange(uuid, Pose.SLEEPING);
          this.checkPoseButtonState(Pose.SLEEPING, ModelPose.DEFAULT);
        }));
    this.spinAttackPoseButton = this.addRenderableWidget(
        menuButton(poseButtonLeft, this.contentTopPos + 144, "spin_attack", button -> {
          NetworkHandler.poseChange(uuid, Pose.SPIN_ATTACK);
          this.checkPoseButtonState(Pose.SPIN_ATTACK, ModelPose.DEFAULT);
        }));
    this.swimmingPoseButton = this.addRenderableWidget(
        menuButton(poseButtonLeft, this.contentTopPos + 168, "swimming", button -> {
          NetworkHandler.poseChange(uuid, Pose.SWIMMING);
          this.checkPoseButtonState(Pose.SWIMMING, ModelPose.DEFAULT);
        }));

    this.checkPoseButtonState(entity.getPose(), entity.getModelPose());
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderEntityAvatar(this.contentLeftPos + 80, this.contentTopPos + 125, 36,
        this.contentLeftPos + 80 - this.xMouse, this.contentTopPos + 65 - this.yMouse, this.entity);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    // Entity
    fill(poseStack, this.contentLeftPos, this.contentTopPos, this.contentLeftPos + 169,
        this.contentTopPos + 187, 0xff000000);
    fill(poseStack, this.contentLeftPos + 1, this.contentTopPos + 1, this.contentLeftPos + 168,
        this.contentTopPos + 186, 0xffaaaaaa);

    // Base
    fill(poseStack, this.contentLeftPos + 1, this.contentTopPos + 125, this.contentLeftPos + 168,
        this.contentTopPos + 186, 0xaa888888);
    fill(poseStack, this.contentLeftPos + 1, this.contentTopPos + 125, this.contentLeftPos + 168,
        this.contentTopPos + 130, 0xaa888888);
  }
}
