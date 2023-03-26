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

package de.markusbordihn.easynpc.client.screen;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.menu.DialogMenu;
import de.markusbordihn.easynpc.network.NetworkMessage;
import de.markusbordihn.easynpc.utils.TextUtils;

@OnlyIn(Dist.CLIENT)
public class DialogScreen extends AbstractContainerScreen<DialogMenu> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Data access
  protected final EasyNPCEntity entity;
  protected final Map<ActionType, ActionData> actions;
  protected final UUID uuid;

  // Internal
  protected Button yesDialogButton = null;
  protected Button noDialogButton = null;
  protected float xMouse;
  protected float yMouse;
  private List<FormattedCharSequence> cachedDialogComponents = Collections.emptyList();

  // Dialog Options
  protected DialogType dialogType = DialogType.BASIC;
  protected String dialog;
  protected MutableComponent dialogComponent;
  protected int numberOfDialogLines = 1;

  public DialogScreen(DialogMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.entity = menu.getEntity();
    this.actions = this.entity.getActions();
    this.uuid = this.entity.getUUID();
  }

  protected void renderDialog(PoseStack poseStack) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DIALOG);

    // Calculate numbers of lines and background shift.
    int minNumberOfLines = Math.max(2, this.numberOfDialogLines);

    // Dialog background according numbers of lines.
    int backgroundHeight = minNumberOfLines * (font.lineHeight + 2);
    int backgroundLeftPos = leftPos + 70;
    int backgroundTopPos = topPos + 15;
    int backgroundTopHeight = Math.min(Math.max(30, backgroundHeight), 105);
    int backgroundBottomHeight = Math.max(backgroundHeight - backgroundTopHeight, 15);
    int backgroundBottomPos = Math.max(116 - backgroundBottomHeight, 15);

    // Render Dialog in two parts to allow smaller and larger dialogs.
    blit(poseStack, backgroundLeftPos, backgroundTopPos, 0, 0, 200, backgroundTopHeight);
    blit(poseStack, backgroundLeftPos, backgroundTopPos + backgroundTopHeight, 0,
        backgroundBottomPos, 200, backgroundBottomHeight);

    // Distribute text for the across the lines.
    if (!this.cachedDialogComponents.isEmpty()) {
      for (int line = 0; line < this.numberOfDialogLines; ++line) {
        FormattedCharSequence formattedCharSequence = this.cachedDialogComponents.get(line);
        this.font.draw(poseStack, formattedCharSequence, leftPos + 87f,
            topPos + 22f + (line * (font.lineHeight + 2)), 0);
      }
    }
  }

  private void setDialog(String text) {
    if (text == null || text.isBlank()) {
      return;
    }

    // Parse dialog Text and replace placeholders.
    Minecraft minecraft = this.minecraft;
    this.dialog =
        DialogUtils.parseDialog(text, this.entity, minecraft != null ? minecraft.player : null);
    this.dialogComponent = Component.literal(this.dialog);

    // Split dialog text to lines.
    this.cachedDialogComponents = this.font.split(this.dialogComponent, 176);
    this.numberOfDialogLines = Math.min(128 / font.lineHeight, this.cachedDialogComponents.size());
  }

  protected static Button menuButton(int left, int top, int width, String label,
      Button.OnPress onPress) {
    return menuButton(left, top, width,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + label), onPress);
  }

  protected static Button menuButton(int left, int top, int width, Component label,
      Button.OnPress onPress) {
    return Button.builder(label, onPress).bounds(left, top, width, 20).build();
  }

  public void closeScreen() {
    Minecraft minecraft = this.minecraft;
    if (minecraft != null) {
      minecraft.setScreen((Screen) null);
    }
  }

  @Override
  public void init() {
    if (this.entity == null) {
      return;
    }
    super.init();

    // Default stats
    this.imageHeight = 200;
    this.imageWidth = 275;

    // Basic Position
    this.titleLabelX = 8;
    this.titleLabelY = 6;
    this.topPos = (this.height - this.imageHeight) / 2;
    this.leftPos = (this.width - this.imageWidth) / 2;

    // Dialog text.
    this.dialogType = this.entity.getDialogType();
    setDialog(this.entity.getDialog());

    // Action for open dialog.
    if (this.actions.containsKey(ActionType.ON_OPEN_DIALOG)) {
      NetworkMessage.triggerAction(this.uuid, ActionType.ON_OPEN_DIALOG);
    }

    // Render additional Buttons for Yes/No Dialog.
    if (this.dialogType == DialogType.YES_NO) {
      int dialogButtonTop = this.topPos + 55 + (numberOfDialogLines * (font.lineHeight));
      String yesDialogButtonText = TextUtils.limitString(this.entity.getYesDialogButton(), 32);
      String noDialogButtonText = TextUtils.limitString(this.entity.getNoDialogButton(), 32);
      boolean smallButtonLayout =
          yesDialogButtonText.length() < 16 && noDialogButtonText.length() < 16;

      this.yesDialogButton = this.addRenderableWidget(menuButton(this.leftPos + 70, dialogButtonTop,
          smallButtonLayout ? 95 : 198, Component.literal(yesDialogButtonText), onPress -> {
            String yesDialogText = this.entity.getYesDialog();
            if (!yesDialogText.isBlank()) {
              setDialog(yesDialogText);
              this.yesDialogButton.visible = false;
              this.noDialogButton.visible = false;
            } else {
              this.closeScreen();
            }

            // Action for yes selection.
            if (this.actions.containsKey(ActionType.ON_YES_SELECTION)) {
              NetworkMessage.triggerAction(this.uuid, ActionType.ON_YES_SELECTION);
            }
          }));

      this.noDialogButton = this.addRenderableWidget(menuButton(
          smallButtonLayout ? this.yesDialogButton.getX() + this.yesDialogButton.getWidth() + 10
              : this.yesDialogButton.getX(),
          smallButtonLayout ? dialogButtonTop : dialogButtonTop + 25, smallButtonLayout ? 95 : 198,
          Component.literal(noDialogButtonText), onPress -> {
            String noDialogText = this.entity.getNoDialog();
            if (!noDialogText.isBlank()) {
              setDialog(noDialogText);
              this.yesDialogButton.visible = false;
              this.noDialogButton.visible = false;
            } else {
              this.closeScreen();
            }

            // Action for no selection.
            if (this.actions.containsKey(ActionType.ON_NO_SELECTION)) {
              NetworkMessage.triggerAction(this.uuid, ActionType.ON_NO_SELECTION);
            }
          }));
    }

  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    if (this.entity == null) {
      return;
    }

    super.render(poseStack, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;

    // Render Avatar
    int avatarPositionTop = 55 + this.entity.getEntityDialogTop();
    int left = this.leftPos + 40;
    int top = this.topPos + 70 + avatarPositionTop;
    ScreenHelper.renderEntityDialog(left, top, Math.round(left - 140 - (this.xMouse * 0.25)),
        Math.round(top - 120 - (this.yMouse * 0.5)), this.entity);

    // Render Dialog
    renderDialog(poseStack);
  }

  @Override
  protected void renderLabels(PoseStack poseStack, int x, int y) {
    this.font.draw(poseStack, this.title, this.titleLabelX, this.titleLabelY, 4210752);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);

    // Main screen
    blit(poseStack, leftPos, topPos, 0, 0, 250, 170);
    blit(poseStack, leftPos + 243, topPos, 215, 0, 35, 170);

    blit(poseStack, leftPos, topPos + 60, 0, 30, 250, 140);
    blit(poseStack, leftPos + 243, topPos + 60, 215, 30, 35, 140);
  }

  @Override
  public void onClose() {
    // Action for close dialog.
    if (this.actions.containsKey(ActionType.ON_CLOSE_DIALOG)) {
      NetworkMessage.triggerAction(this.uuid, ActionType.ON_CLOSE_DIALOG);
    }
    super.onClose();
  }

}
