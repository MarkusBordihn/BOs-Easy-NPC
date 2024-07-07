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

import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.player.Inventory;

public class DefaultPoseConfigurationScreen<T extends ConfigurationMenu>
    extends de.markusbordihn.easynpc.client.screen.configuration.pose.PoseConfigurationScreen<T> {

  public static final int BUTTON_WIDTH = 100;
  protected Button crouchingPoseButton;
  protected Button dyingPoseButton;
  protected Button fallFlyingPoseButton;
  protected Button longJumpPoseButton;
  protected Button sleepingPoseButton;
  protected Button spinAttackPoseButton;
  protected Button standingPoseButton;
  protected Button swimmingPoseButton;

  public DefaultPoseConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void checkPoseButtonState(Pose pose, ModelPose modelPose) {
    Pose currentPose = pose != null ? pose : this.getEasyNPCEntity().getPose();
    boolean isCustomModelPose =
        (modelPose != null ? modelPose : this.modelData.getModelPose()) == ModelPose.CUSTOM;
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

    // Pose Buttons
    int poseButtonLeft = this.contentLeftPos + 175;
    this.standingPoseButton =
        this.addRenderableWidget(
            new TextButton(
                poseButtonLeft,
                this.contentTopPos,
                BUTTON_WIDTH,
                "pose.standing",
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .poseChange(this.getNpcUUID(), Pose.STANDING);
                  this.checkPoseButtonState(Pose.STANDING, ModelPose.DEFAULT);
                }));
    this.crouchingPoseButton =
        this.addRenderableWidget(
            new TextButton(
                poseButtonLeft,
                this.contentTopPos + 24,
                BUTTON_WIDTH,
                "pose.crouching",
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .poseChange(this.getNpcUUID(), Pose.CROUCHING);
                  this.checkPoseButtonState(Pose.CROUCHING, ModelPose.DEFAULT);
                }));
    this.dyingPoseButton =
        this.addRenderableWidget(
            new TextButton(
                poseButtonLeft,
                this.contentTopPos + 48,
                BUTTON_WIDTH,
                "pose.dying",
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .poseChange(this.getNpcUUID(), Pose.DYING);
                  this.checkPoseButtonState(Pose.DYING, ModelPose.DEFAULT);
                }));
    this.fallFlyingPoseButton =
        this.addRenderableWidget(
            new TextButton(
                poseButtonLeft,
                this.contentTopPos + 72,
                BUTTON_WIDTH,
                "pose.fall_flying",
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .poseChange(this.getNpcUUID(), Pose.FALL_FLYING);
                  this.checkPoseButtonState(Pose.FALL_FLYING, ModelPose.DEFAULT);
                }));
    this.longJumpPoseButton =
        this.addRenderableWidget(
            new TextButton(
                poseButtonLeft,
                this.contentTopPos + 96,
                BUTTON_WIDTH,
                "pose.long_jumping",
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .poseChange(this.getNpcUUID(), Pose.LONG_JUMPING);
                  this.checkPoseButtonState(Pose.LONG_JUMPING, ModelPose.DEFAULT);
                }));
    this.sleepingPoseButton =
        this.addRenderableWidget(
            new TextButton(
                poseButtonLeft,
                this.contentTopPos + 120,
                BUTTON_WIDTH,
                "pose.sleeping",
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .poseChange(this.getNpcUUID(), Pose.SLEEPING);
                  this.checkPoseButtonState(Pose.SLEEPING, ModelPose.DEFAULT);
                }));
    this.spinAttackPoseButton =
        this.addRenderableWidget(
            new TextButton(
                poseButtonLeft,
                this.contentTopPos + 144,
                BUTTON_WIDTH,
                "pose.spin_attack",
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .poseChange(this.getNpcUUID(), Pose.SPIN_ATTACK);
                  this.checkPoseButtonState(Pose.SPIN_ATTACK, ModelPose.DEFAULT);
                }));
    this.swimmingPoseButton =
        this.addRenderableWidget(
            new TextButton(
                poseButtonLeft,
                this.contentTopPos + 168,
                BUTTON_WIDTH,
                "pose.swimming",
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .poseChange(this.getNpcUUID(), Pose.SWIMMING);
                  this.checkPoseButtonState(Pose.SWIMMING, ModelPose.DEFAULT);
                }));

    this.checkPoseButtonState(this.getEasyNPCEntity().getPose(), this.modelData.getModelPose());
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Backup entity information
    boolean entityInvisible = this.getEasyNPCEntity().isInvisible();

    // Adjust entity information for rendering
    this.getEasyNPCEntity().setInvisible(false);

    // Render Entity
    ScreenHelper.renderScaledEntityAvatar(
        this.contentLeftPos + 80,
        this.contentTopPos + 145,
        36,
        this.contentLeftPos + 80 - this.xMouse,
        this.contentTopPos + 85 - this.yMouse,
        this.getEasyNPC(),
        this.getEasyNPC().getEasyNPCScaleData(),
        this.getEasyNPC().getEasyNPCModelData());

    // Restore entity information
    this.getEasyNPCEntity().setInvisible(entityInvisible);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    // Entity
    guiGraphics.fill(
        this.contentLeftPos,
        this.contentTopPos,
        this.contentLeftPos + 169,
        this.contentTopPos + 207,
        0xff000000);
    guiGraphics.fill(
        this.contentLeftPos + 1,
        this.contentTopPos + 1,
        this.contentLeftPos + 168,
        this.contentTopPos + 206,
        0xffaaaaaa);

    // Base
    guiGraphics.fill(
        this.contentLeftPos + 1,
        this.contentTopPos + 145,
        this.contentLeftPos + 168,
        this.contentTopPos + 206,
        0xaa888888);
    guiGraphics.fill(
        this.contentLeftPos + 1,
        this.contentTopPos + 145,
        this.contentLeftPos + 168,
        this.contentTopPos + 150,
        0xaa888888);
  }
}
