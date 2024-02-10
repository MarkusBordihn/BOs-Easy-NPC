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

package de.markusbordihn.easynpc.client.screen.editor;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.CloseButton;
import de.markusbordihn.easynpc.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogTextData;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.editor.DialogTextEditorMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import javax.annotation.Nonnull;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@OnlyIn(Dist.CLIENT)
public class DialogTextEditorScreen extends AbstractContainerScreen<DialogTextEditorMenu> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final int MAX_NUMBER_OF_DIALOG_TEXTS = 6;
  protected final ClientLevel clientLevel;
  protected final LocalPlayer localPlayer;
  protected final Minecraft minecraftInstance;
  protected final DialogDataSet dialogDataSet;
  protected final DialogDataEntry dialogData;
  protected final Set<DialogTextData> dialogTexts;
  protected final EasyNPCEntity entity;
  protected final UUID uuid;
  protected final ConfigurationType formerConfigurationType;
  protected final UUID dialogId;
  private final Set<TextField> dialogTextFields = new HashSet<>();
  protected Button homeButton;
  protected Button dialogButton;
  protected Button dialogTextButton;
  protected Button closeButton;
  protected Button saveButton;
  protected Button cancelButton;
  protected Checkbox translateCheckbox;
  protected int bottomPos;
  protected float xMouse;
  protected float yMouse;
  protected int rightPos;

  @OnlyIn(Dist.CLIENT)
  public DialogTextEditorScreen(
      DialogTextEditorMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);

    // Data access
    this.uuid = menu.getUUID();
    this.entity = menu.getEntity();
    this.dialogDataSet = menu.getDialogDataSet();
    this.dialogData = menu.getDialogData();
    this.dialogTexts = dialogData.getDialogTexts();
    this.dialogId = menu.getDialogId();
    this.formerConfigurationType = menu.getFormerConfigurationType();

    // General environment Data
    this.minecraftInstance = Minecraft.getInstance();
    this.localPlayer = this.minecraftInstance.player;
    this.clientLevel = this.minecraftInstance.level;
  }

  private void openPreviousScreen() {
    if (this.formerConfigurationType == ConfigurationType.DIALOG_EDITOR) {
      NetworkMessageHandler.openDialogEditor(uuid, this.dialogId, this.formerConfigurationType);
    } else if (this.formerConfigurationType == ConfigurationType.ADVANCED_DIALOG) {
      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.ADVANCED_DIALOG);
    } else if (this.formerConfigurationType != null) {
      NetworkMessageHandler.openConfiguration(uuid, this.formerConfigurationType);
    } else if (dialogDataSet.getType() == DialogType.YES_NO) {
      NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.YES_NO_DIALOG);
    } else {
      this.closeScreen();
    }
  }

  private void closeScreen() {
    if (this.minecraftInstance != null) {
      this.minecraftInstance.setScreen(null);
    }
  }

  protected void saveDialogData() {
    // Save dialog texts
    Set<DialogTextData> validDialogTexts = new HashSet<>();
    for (TextField textfield : dialogTextFields) {
      String text = textfield.getValue();
      if (!text.isEmpty()) {
        validDialogTexts.add(new DialogTextData(text, translateCheckbox.selected()));
      }
    }
    this.dialogData.setDialogTexts(validDialogTexts);

    // Save dialog data
    NetworkMessageHandler.saveDialog(this.uuid, this.dialogId, this.dialogData);
  }

  @Override
  public void init() {
    super.init();

    // Default stats
    this.imageHeight = 243;
    this.imageWidth = 318;

    // Core Positions
    this.titleLabelX = 8;
    this.titleLabelY = 7;
    this.topPos = ((this.height - this.imageHeight) / 2) + 2;
    this.leftPos = (this.width - this.imageWidth) / 2;
    this.rightPos = this.leftPos + this.imageWidth;
    this.bottomPos = this.topPos + this.imageHeight;

    // Allow repeated key events
    if (this.minecraft != null) {
      this.minecraft.keyboardHandler.setSendRepeatsToGui(true);
    }

    // Home Button
    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 7,
                this.topPos + 7,
                10,
                18,
                "<",
                onPress -> this.openPreviousScreen()));

    // Dialog Button
    this.dialogButton =
        this.addRenderableWidget(
            new DialogButton(
                this.homeButton.x + this.homeButton.getWidth(),
                this.topPos + 7,
                140,
                this.dialogData.getName(21),
                onPress -> this.openPreviousScreen()));

    // Dialog Text Button
    this.dialogTextButton =
        this.addRenderableWidget(
            new DialogButton(
                this.dialogButton.x + this.dialogButton.getWidth(),
                this.topPos + 7,
                140,
                "Dialog Text",
                onPress -> {
                }));
    this.dialogTextButton.active = false;

    // Dialog Texts
    int dialogTextIndex = 0;
    int dialogTextLeftPos = this.leftPos + 20;
    int dialogTextTopPos = this.topPos + 50;
    int dialogTextWidth = 290;
    boolean dialogTextTranslatable = false;

    // Get stored dialog texts
    for (DialogTextData dialogText : dialogTexts) {
      if (dialogTextIndex >= MAX_NUMBER_OF_DIALOG_TEXTS) {
        break;
      }
      TextField textfield =
          this.addRenderableWidget(
              new TextField(
                  this.font,
                  dialogTextLeftPos,
                  dialogTextTopPos + dialogTextIndex * 20,
                  dialogTextWidth,
                  dialogText.getText(),
                  512));
      this.dialogTextFields.add(textfield);
      if (dialogText.getTranslate()) {
        dialogTextTranslatable = true;
      }
      dialogTextIndex++;
    }

    // Add additional dialog texts, if needed.
    for (int i = dialogTextIndex; i < MAX_NUMBER_OF_DIALOG_TEXTS; i++) {
      TextField textfield =
          this.addRenderableWidget(
              new TextField(
                  this.font,
                  dialogTextLeftPos,
                  dialogTextTopPos + i * 20,
                  dialogTextWidth,
                  "",
                  512));
      this.dialogTextFields.add(textfield);
    }

    // Dialog Translate
    this.translateCheckbox =
        new Checkbox(
            this.leftPos + 20, this.bottomPos - 70, "dialog.translate", dialogTextTranslatable);
    this.addRenderableWidget(this.translateCheckbox);

    // Close Button
    this.closeButton =
        this.addRenderableWidget(
            new CloseButton(this.rightPos - 15, this.topPos + 4, onPress -> closeScreen()));

    // Save Button
    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.leftPos + 25,
                this.bottomPos - 35,
                85,
                "save",
                onPress -> {
                  // Save dialog data
                  this.saveDialogData();

                  // Return back to the simple yes and no dialog editor or the full dialog editor.
                  openPreviousScreen();
                }));

    // Chancel Button
    this.cancelButton =
        this.addRenderableWidget(
            new CancelButton(
                this.saveButton.x + 95 + this.saveButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                "cancel",
                onPress -> this.openPreviousScreen()));
  }

  protected void renderEditLabels(PoseStack poseStack) {
    Text.drawConfigString(
        poseStack,
        this.font,
        "dialog.text",
        leftPos + 10,
        this.topPos + 34,
        Constants.FONT_COLOR_BLACK);

    for (int i = 1; i <= MAX_NUMBER_OF_DIALOG_TEXTS; i++) {
      Text.drawString(
          poseStack,
          this.font,
          i + ":",
          leftPos + 10,
          this.topPos + 35 + i * 20,
          Constants.FONT_COLOR_BLACK);
    }
  }

  @Override
  public void render(@Nonnull PoseStack poseStack, int x, int y, float partialTicks) {
    this.renderBackground(poseStack);
    super.render(poseStack, x, y, partialTicks);
    this.renderEditLabels(poseStack);
    this.xMouse = x;
    this.yMouse = y;
  }

  @Override
  protected void renderLabels(@Nonnull PoseStack poseStack, int x, int y) {
    // No labels
  }

  @Override
  protected void renderBg(
      @Nonnull PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);

    // Main screen: top left
    this.blit(poseStack, leftPos, topPos, 0, 0, 210, 160);

    // Main screen: top right
    this.blit(poseStack, leftPos + 203, topPos, 132, 0, 120, 160);

    // Main screen: bottom left
    this.blit(poseStack, leftPos, topPos + 77, 0, 5, 210, 170);

    // Main screen: bottom right
    this.blit(poseStack, leftPos + 203, topPos + 77, 132, 5, 120, 170);
  }

  @Override
  public void removed() {
    if (this.minecraft != null) {
      this.minecraft.keyboardHandler.setSendRepeatsToGui(false);
    }
    super.removed();
  }

  @Override
  public boolean keyPressed(int keyCode, int unused1, int unused2) {
    if (keyCode != 257 && keyCode != 335 && keyCode != 69 && keyCode != 73) {
      return super.keyPressed(keyCode, unused1, unused2);
    }
    return keyCode == 257 || keyCode == 335 || keyCode == 73;
  }
}
