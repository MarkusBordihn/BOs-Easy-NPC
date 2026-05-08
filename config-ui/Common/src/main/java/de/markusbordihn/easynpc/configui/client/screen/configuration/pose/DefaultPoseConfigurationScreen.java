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
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.player.Inventory;

public class DefaultPoseConfigurationScreen<T extends ConfigurationMenu>
    extends PoseConfigurationScreen<T> {

  public static final int BUTTON_WIDTH = 75;
  private static final int BUTTON_HEIGHT = 16;
  private static final int BUTTON_SPACING = 18;
  private static final int COLUMNS = 2;
  private static final int COLUMN_SPACING = 4;
  private static final int MAX_VISIBLE_ROWS = 9;
  private static final int MAX_VISIBLE_BUTTONS = MAX_VISIBLE_ROWS * COLUMNS;
  private static final int POSE_LIST_START_Y = 20;

  private final List<Button> poseButtons = new ArrayList<>();
  private final List<ResourceLocation> poseKeys = new ArrayList<>();
  private int scrollOffset = 0;
  private Button resetPoseButton;

  public DefaultPoseConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void updatePoseButtonStates() {
    String currentPoseName = this.modelData.getModelPoseName();
    if (resetPoseButton != null) {
      resetPoseButton.active = this.modelData.getModelPose() != ModelPose.VANILLA;
    }
    for (int i = 0; i < poseButtons.size(); i++) {
      int globalIndex = i + scrollOffset;
      if (globalIndex < poseKeys.size()) {
        ResourceLocation poseId = poseKeys.get(globalIndex);
        poseButtons.get(i).active = !poseId.toString().equals(currentPoseName);
      }
    }
  }

  @Override
  public boolean mouseScrolled(double mouseX, double mouseY, double scrollX, double scrollY) {
    if (poseKeys.size() > MAX_VISIBLE_BUTTONS) {
      int totalRows = (poseKeys.size() + COLUMNS - 1) / COLUMNS;
      int maxScrollOffset = Math.max(0, totalRows - MAX_VISIBLE_ROWS) * COLUMNS;
      int newOffset = scrollOffset - (int) Math.signum(scrollY) * COLUMNS;
      int clamped = Math.max(0, Math.min(newOffset, maxScrollOffset));
      if (clamped != scrollOffset) {
        scrollOffset = clamped;
        this.rebuildWidgets();
      }
      return true;
    }
    return super.mouseScrolled(mouseX, mouseY, scrollX, scrollY);
  }

  @Override
  public void init() {
    super.init();

    this.defaultPoseButton.active = false;

    // Discover available poses for this NPC's skin model
    SkinModel skinModel = this.getSkinModel();

    // Prefer server-synced pose list (required for dedicated server setups)
    Set<ResourceLocation> availablePoses = null;
    if (this.getAdditionalScreenData() != null) {
      ListTag poseKeysTag = this.getAdditionalScreenData().getList("PoseKeys");
      if (poseKeysTag != null && !poseKeysTag.isEmpty()) {
        availablePoses =
            CompoundTagUtils.readResourceLocations(poseKeysTag).stream()
                .sorted(
                    (a, b) -> {
                      boolean aStanding = a.getPath().endsWith("/standing");
                      boolean bStanding = b.getPath().endsWith("/standing");
                      if (aStanding && !bStanding) return -1;
                      if (!aStanding && bStanding) return 1;
                      return a.getPath().compareTo(b.getPath());
                    })
                .collect(Collectors.toCollection(LinkedHashSet::new));
      }
    }

    // Fallback to local PoseManager
    if (availablePoses == null || availablePoses.isEmpty()) {
      availablePoses =
          skinModel != null
              ? PoseManager.getPoseDataKeysForModel(skinModel)
              : PoseManager.getPoseDataKeys();
    }

    poseKeys.clear();
    poseKeys.addAll(availablePoses);
    poseButtons.clear();

    int poseButtonLeft = this.contentLeftPos + 160;
    int col2Left = poseButtonLeft + BUTTON_WIDTH + COLUMN_SPACING;
    int resetButtonWidth = BUTTON_WIDTH * COLUMNS + COLUMN_SPACING;

    // Reset Pose Button (spans both columns)
    this.resetPoseButton =
        this.addRenderableWidget(
            new TextButton(
                poseButtonLeft,
                this.contentTopPos,
                resetButtonWidth,
                Component.translatable("text.easy_npc.config.pose.reset"),
                button -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .poseChange(this.getEasyNPCUUID(), Pose.STANDING);
                  this.modelData.setModelPose(ModelPose.VANILLA);
                  this.modelData.setModelPoseName("");
                  this.modelData.setModelPartRotation(new EnumMap<>(ModelPartType.class));
                  this.modelData.setModelPartPosition(new EnumMap<>(ModelPartType.class));
                  if (this.lockRotationCheckbox != null && this.lockRotationCheckbox.selected()) {
                    this.lockRotationCheckbox.setSelected(false);
                    NetworkMessageHandlerManager.getServerHandler()
                        .modelRotationChange(
                            this.getEasyNPCUUID(),
                            ModelPartType.ROOT,
                            this.modelData.getModelRootData().rotation().withLocked(false));
                  }
                  this.updatePoseButtonStates();
                }));

    // Pose buttons in two columns, mouse-wheel-scrollable
    int visibleCount = Math.min(poseKeys.size() - scrollOffset, MAX_VISIBLE_BUTTONS);
    for (int i = 0; i < visibleCount; i++) {
      int globalIndex = i + scrollOffset;
      ResourceLocation poseId = poseKeys.get(globalIndex);
      String displayName = PoseManager.getPoseDisplayName(poseId);

      int row = i / COLUMNS;
      int col = i % COLUMNS;
      int x = col == 0 ? poseButtonLeft : col2Left;
      int y = this.contentTopPos + POSE_LIST_START_Y + row * BUTTON_SPACING;

      Button button =
          this.addRenderableWidget(
              new TextButton(
                  x,
                  y,
                  BUTTON_WIDTH,
                  Component.literal(displayName),
                  btn -> {
                    NetworkMessageHandlerManager.getServerHandler()
                        .namedPoseChange(this.getEasyNPCUUID(), poseId);
                    this.modelData.setModelPoseName(poseId.toString());
                    if (this.lockRotationCheckbox != null
                        && !this.lockRotationCheckbox.selected()) {
                      this.lockRotationCheckbox.setSelected(true);
                      NetworkMessageHandlerManager.getServerHandler()
                          .modelRotationChange(
                              this.getEasyNPCUUID(),
                              ModelPartType.ROOT,
                              this.modelData.getModelRootData().rotation().withLocked(true));
                    }
                    this.updatePoseButtonStates();
                  }));
      poseButtons.add(button);
    }

    this.updatePoseButtonStates();

    // Animation Behavior Button
    this.addRenderableWidget(
        this.createAnimationBehaviorButton(this.contentLeftPos + 30, this.contentTopPos + 190));

    // Follow Cursor Toggle Button
    this.createFollowCursorToggleButton(this.contentLeftPos + 134, this.contentTopPos);

    // Lock Rotation Checkbox
    this.createLockRotationCheckbox(this.contentLeftPos + 160, this.contentTopPos + 190);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Backup entity information
    boolean entityInvisible = this.getEasyNPCEntity().isInvisible();

    // Adjust entity information for rendering
    this.getEasyNPCEntity().setInvisible(false);

    // Render Entity
    EntityRenderConfig renderConfig =
        EntityRenderConfig.guiScaled(this.contentLeftPos + 70, this.contentTopPos + 140, 36);
    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        renderConfig,
        this.getPreviewRotationYaw(this.xMouse, renderConfig),
        this.getPreviewRotationPitch(this.yMouse, renderConfig));

    // Restore entity information
    this.getEasyNPCEntity().setInvisible(entityInvisible);

    // Scroll indicator when more poses exist than the visible window
    if (poseKeys.size() > MAX_VISIBLE_BUTTONS) {
      int totalRows = (poseKeys.size() + COLUMNS - 1) / COLUMNS;
      int currentRow = scrollOffset / COLUMNS;
      String indicator =
          "\u2195 "
              + (currentRow + 1)
              + "-"
              + Math.min(currentRow + MAX_VISIBLE_ROWS, totalRows)
              + " / "
              + totalRows;
      guiGraphics.drawString(
          this.font,
          indicator,
          this.contentLeftPos + 175,
          this.contentTopPos + 182,
          0xAAAAAA,
          false);
    }
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    // Entity
    guiGraphics.fill(
        this.contentLeftPos,
        this.contentTopPos,
        this.contentLeftPos + 154,
        this.contentTopPos + 207,
        0xff000000);
    guiGraphics.fill(
        this.contentLeftPos + 1,
        this.contentTopPos + 1,
        this.contentLeftPos + 153,
        this.contentTopPos + 206,
        0xffaaaaaa);

    // Base
    guiGraphics.fill(
        this.contentLeftPos + 1,
        this.contentTopPos + 145,
        this.contentLeftPos + 153,
        this.contentTopPos + 206,
        0xaa888888);
    guiGraphics.fill(
        this.contentLeftPos + 1,
        this.contentTopPos + 145,
        this.contentLeftPos + 153,
        this.contentTopPos + 150,
        0xaa888888);

    // Animation label
    Text.drawConfigString(
        guiGraphics, this.font, "animation", this.contentLeftPos + 46, this.contentTopPos + 179);
  }
}
