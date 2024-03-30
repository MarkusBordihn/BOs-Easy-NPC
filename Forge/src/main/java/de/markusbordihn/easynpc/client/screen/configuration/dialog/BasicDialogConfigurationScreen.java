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

package de.markusbordihn.easynpc.client.screen.configuration.dialog;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.menu.configuration.dialog.BasicDialogConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import java.util.Collections;
import java.util.List;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class BasicDialogConfigurationScreen
    extends DialogConfigurationScreen<BasicDialogConfigurationMenu> {

  protected EditBox dialogBox;
  protected Button saveButton = null;
  protected int numberOfTextLines = 1;
  private List<FormattedCharSequence> textComponents = Collections.emptyList();
  private String dialogValue = "";

  public BasicDialogConfigurationScreen(
      BasicDialogConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.basicDialogButton.active = false;

    // Dialog
    this.dialogValue =
        dialogDataSet.hasDialog() && dialogDataSet.getType() == DialogType.BASIC
            ? dialogDataSet.getDefaultDialog().getText()
            : "";
    this.dialogBox = new TextField(this.font, this.contentLeftPos, this.topPos + 60, 300);
    this.dialogBox.setMaxLength(512);
    this.dialogBox.setValue(this.dialogValue);
    this.addRenderableWidget(this.dialogBox);

    // Save Button
    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.contentLeftPos + 26,
                this.bottomPos - 40,
                "save",
                onPress -> {
                  DialogDataSet dialogDataSet =
                      DialogUtils.getBasicDialog(this.dialogBox.getValue());
                  NetworkMessageHandler.saveDialog(uuid, dialogDataSet);
                  this.dialogValue = this.dialogBox.getValue();
                }));

    // Chancel Button
    this.addRenderableWidget(
        new CancelButton(
            this.rightPos - 130, this.bottomPos - 40, "cancel", onPress -> this.showMainScreen()));

    // Pre-format text
    this.textComponents =
        this.font.split(
            Component.translatable(Constants.TEXT_CONFIG_PREFIX + "dialog_placeholder"),
            this.imageWidth - 20);
    this.numberOfTextLines = this.textComponents.size();
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    Text.drawConfigString(
        poseStack, this.font, "dialog_text", this.contentLeftPos, this.topPos + 50);

    if (!this.textComponents.isEmpty()) {
      for (int line = 0; line < this.numberOfTextLines; ++line) {
        FormattedCharSequence formattedCharSequence = this.textComponents.get(line);
        Text.drawString(
            poseStack,
            this.font,
            formattedCharSequence,
            leftPos + 15,
            topPos + 100 + (line * (font.lineHeight + 2)));
      }
    }
  }

  @Override
  public void containerTick() {
    super.containerTick();

    if (saveButton != null) {
      saveButton.active = !dialogBox.getValue().equals(dialogValue);
    }
  }
}
