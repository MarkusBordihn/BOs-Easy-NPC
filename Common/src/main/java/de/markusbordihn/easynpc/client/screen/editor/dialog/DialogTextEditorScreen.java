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

package de.markusbordihn.easynpc.client.screen.editor.dialog;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.EditorScreen;
import de.markusbordihn.easynpc.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogTextData;
import de.markusbordihn.easynpc.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.HashSet;
import java.util.Set;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class DialogTextEditorScreen<T extends EditorMenu> extends EditorScreen<T> {

  private static final int MAX_NUMBER_OF_DIALOG_TEXTS = 6;
  private final Set<TextField> dialogTextFields = new HashSet<>();
  protected Button homeButton;
  protected Button dialogButton;
  protected Button dialogTextButton;
  protected Button saveButton;
  protected Button cancelButton;

  public DialogTextEditorScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  protected void saveDialogData() {
    // Save dialog texts
    Set<DialogTextData> validDialogTexts = new HashSet<>();
    for (TextField textfield : dialogTextFields) {
      String text = textfield.getValue();
      if (!text.isEmpty()) {
        validDialogTexts.add(new DialogTextData(text));
      }
    }

    DialogDataEntry dialogDataEntry = this.getDialogData(this.getDialogUUID());
    dialogDataEntry.setDialogTexts(validDialogTexts);

    // Save dialog data
    NetworkMessageHandlerManager.getServerHandler()
        .saveDialog(this.getEasyNPCUUID(), this.getDialogUUID(), dialogDataEntry);
  }

  @Override
  public void init() {
    super.init();

    // Home Button
    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 7,
                this.topPos + 7,
                10,
                18,
                "<",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.DIALOG)));

    // Dialog Button
    this.dialogButton =
        this.addRenderableWidget(
            new DialogButton(
                this.homeButton.x + this.homeButton.getWidth(),
                this.topPos + 7,
                140,
                this.getDialogData().getName(21),
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openDialogEditor(this.getEasyNPCUUID(), this.getDialogUUID())));

    // Dialog Text Button
    this.dialogTextButton =
        this.addRenderableWidget(
            new DialogButton(
                this.dialogButton.x + this.dialogButton.getWidth(),
                this.topPos + 7,
                140,
                "Dialog Text",
                onPress -> {}));
    this.dialogTextButton.active = false;

    // Dialog Texts
    int dialogTextIndex = 0;
    int dialogTextLeftPos = this.leftPos + 20;
    int dialogTextTopPos = this.topPos + 50;
    int dialogTextWidth = 290;

    // Get stored dialog texts
    Set<DialogTextData> dialogTexts = this.getDialogData(this.getDialogUUID()).getDialogTexts();
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
                  dialogText.text(),
                  512));
      this.dialogTextFields.add(textfield);
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

    // Save Button
    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.leftPos + 25,
                this.bottomPos - 35,
                85,
                "save",
                onPress -> {
                  this.saveDialogData();
                  NetworkMessageHandlerManager.getServerHandler()
                      .openDialogEditor(this.getEasyNPCUUID(), this.getDialogUUID());
                }));

    // Chancel Button
    this.cancelButton =
        this.addRenderableWidget(
            new CancelButton(
                this.saveButton.x + 95 + this.saveButton.getWidth() + 10,
                this.bottomPos - 35,
                85,
                "cancel",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.DIALOG)));
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
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    this.renderBackground(poseStack);
    super.render(poseStack, x, y, partialTicks);
    this.renderEditLabels(poseStack);
  }
}
