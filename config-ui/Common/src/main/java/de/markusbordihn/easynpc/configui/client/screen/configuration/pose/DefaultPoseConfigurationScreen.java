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

package de.markusbordihn.easynpc.configui.client.screen.configuration.pose;

import de.markusbordihn.easynpc.client.pose.PoseManager;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

public class DefaultPoseConfigurationScreen<T extends ConfigurationMenu>
    extends PoseConfigurationScreen<T> {

  public static final int BUTTON_WIDTH = 100;
  private static final int BUTTON_HEIGHT = 16;
  private static final int BUTTON_SPACING = 20;
  private static final int MAX_VISIBLE_BUTTONS = 10;

  private final List<Button> poseButtons = new ArrayList<>();
  private final List<ResourceLocation> poseKeys = new ArrayList<>();
  private int scrollOffset = 0;

  public DefaultPoseConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void updatePoseButtonStates() {
    String currentPoseName = this.modelData.getModelPoseName();
    for (int i = 0; i < poseButtons.size(); i++) {
      if (i < poseKeys.size()) {
        ResourceLocation poseId = poseKeys.get(i + scrollOffset);
        poseButtons.get(i).active = !poseId.toString().equals(currentPoseName);
      }
    }
  }

  @Override
  public void init() {
    super.init();

    this.defaultPoseButton.active = false;

    // Discover available poses for this NPC's skin model
    SkinModel skinModel = this.getSkinModel();
    Set<ResourceLocation> availablePoses =
        skinModel != null
            ? PoseManager.getPoseDataKeysForModel(skinModel)
            : PoseManager.getPoseDataKeys();

    poseKeys.clear();
    poseKeys.addAll(availablePoses);
    poseButtons.clear();

    int poseButtonLeft = this.contentLeftPos + 175;
    int maxButtons = Math.min(poseKeys.size(), MAX_VISIBLE_BUTTONS);
    int standingSeparatorOffset = BUTTON_SPACING / 2;

    for (int i = 0; i < maxButtons; i++) {
      ResourceLocation poseId = poseKeys.get(i + scrollOffset);
      String displayName = PoseManager.getPoseDisplayName(poseId);

      // Add extra vertical offset after the standing button (index 0)
      int yOffset = i * BUTTON_SPACING;
      if (i > 0) {
        yOffset += standingSeparatorOffset;
      }

      Button button =
          this.addRenderableWidget(
              new TextButton(
                  poseButtonLeft,
                  this.contentTopPos + yOffset,
                  BUTTON_WIDTH,
                  Component.literal(displayName),
                  btn -> {
                    NetworkMessageHandlerManager.getServerHandler()
                        .namedPoseChange(this.getEasyNPCUUID(), poseId);
                    this.modelData.setModelPoseName(poseId.toString());
                    this.updatePoseButtonStates();
                  }));
      poseButtons.add(button);
    }

    this.updatePoseButtonStates();

    // Animation Behavior Button
    this.addRenderableWidget(
        this.createAnimationBehaviorButton(this.contentLeftPos + 30, this.contentTopPos + 190));

    // Follow Cursor Toggle Button
    this.createFollowCursorToggleButton(this.contentLeftPos + 149, this.contentTopPos);

    // Lock Rotation Checkbox
    this.createLockRotationCheckbox(this.contentLeftPos + 175, this.contentTopPos + 190);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Backup entity information
    boolean entityInvisible = this.getEasyNPCEntity().isInvisible();

    // Adjust entity information for rendering
    this.getEasyNPCEntity().setInvisible(false);

    // Render Entity
    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.guiScaled(
            this.contentLeftPos + 80,
            this.contentTopPos + 145,
            36,
            this.getPreviewRotationYaw(this.contentLeftPos + 80 - this.xMouse),
            this.getPreviewRotationPitch(this.contentTopPos + 85 - this.yMouse)));

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

    // Animation label
    Text.drawConfigString(
        guiGraphics, this.font, "animation", this.contentLeftPos + 46, this.contentTopPos + 179);
  }
}
