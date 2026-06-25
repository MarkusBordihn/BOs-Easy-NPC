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

package de.markusbordihn.easynpc.configui.client.screen.editor.dialog;

import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.EditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.components.CancelButton;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.DialogButton;
import de.markusbordihn.easynpc.configui.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.dialog.DialogButtonConditionMode;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogOptionsData;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class DialogOptionsEditorScreen<T extends EditorMenu> extends EditorScreen<T> {

  private static final int SLIDER_WIDTH = 250;
  private static final int SLIDER_HEIGHT = 14;

  protected Button homeButton;
  protected Button dialogButton;
  protected Button saveButton;
  protected Button cancelButton;
  protected Checkbox allowEscCloseCheckbox;
  protected Checkbox showCloseButtonCheckbox;
  protected Checkbox displayAvatarCheckbox;
  protected Checkbox hideUnavailableButtonsCheckbox;
  protected RangeSliderButton avatarScaleSlider;
  protected RangeSliderButton avatarTopSlider;
  protected RangeSliderButton avatarLeftSlider;
  private boolean allowEscCloseValue = true;
  private boolean showCloseButtonValue = true;
  private boolean displayAvatarValue = true;
  private boolean hideUnavailableButtonsValue = false;
  private int npcDefaultScale;
  private int npcDefaultTop;
  private int npcDefaultLeft;
  private int avatarScaleInitialValue;
  private int avatarTopInitialValue;
  private int avatarLeftInitialValue;

  public DialogOptionsEditorScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void openPreviousScreen() {
    NetworkMessageHandlerManager.getServerHandler()
        .openDialogEditor(this.getEasyNPCUUID(), this.getDialogUUID());
  }

  private void setAvatarSlidersActive(boolean active) {
    this.avatarScaleSlider.active = active;
    this.avatarTopSlider.active = active;
    this.avatarLeftSlider.active = active;
  }

  private void saveDialogData() {
    boolean currentAllowEscClose = this.allowEscCloseCheckbox.selected();
    boolean currentShowCloseButton = this.showCloseButtonCheckbox.selected();
    boolean currentDisplayAvatar = this.displayAvatarCheckbox.selected();
    boolean currentHideUnavailableButtons = this.hideUnavailableButtonsCheckbox.selected();
    int currentScale = Math.round(this.avatarScaleSlider.getTargetValue());
    int currentTop = Math.round(this.avatarTopSlider.getTargetValue());
    int currentLeft = Math.round(this.avatarLeftSlider.getTargetValue());

    boolean hasChanged =
        currentAllowEscClose != this.allowEscCloseValue
            || currentShowCloseButton != this.showCloseButtonValue
            || currentDisplayAvatar != this.displayAvatarValue
            || currentHideUnavailableButtons != this.hideUnavailableButtonsValue
            || currentScale != this.avatarScaleInitialValue
            || currentTop != this.avatarTopInitialValue
            || currentLeft != this.avatarLeftInitialValue;
    if (!hasChanged) {
      return;
    }

    DialogDataEntry dialogDataEntry = this.getDialogData();
    dialogDataEntry.setDialogOptions(
        new DialogOptionsData(
            currentAllowEscClose,
            currentShowCloseButton,
            currentDisplayAvatar,
            currentTop != this.npcDefaultTop ? currentTop : null,
            currentLeft != this.npcDefaultLeft ? currentLeft : null,
            currentScale != this.npcDefaultScale ? currentScale : null,
            currentHideUnavailableButtons
                ? DialogButtonConditionMode.HIDE
                : DialogButtonConditionMode.LOCK));

    NetworkMessageHandlerManager.getServerHandler()
        .saveDialog(this.getEasyNPCUUID(), this.getDialogUUID(), dialogDataEntry);
  }

  @Override
  public void init() {
    super.init();

    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 7,
                this.topPos + 7,
                10,
                16,
                "<",
                onPress -> this.openPreviousScreen()));

    this.dialogButton =
        this.addRenderableWidget(
            new DialogButton(
                this.homeButton.getX() + this.homeButton.getWidth(),
                this.topPos + 7,
                140,
                this.getDialogData().getName(21),
                onPress -> {}));
    this.dialogButton.active = false;

    DialogOptionsData dialogOptions = this.getDialogData().getDialogOptions();
    this.npcDefaultScale = this.getEasyNPC().getEasyNPCDialogData().getEntityDialogScaling();
    this.npcDefaultTop = this.getEasyNPC().getEasyNPCDialogData().getEntityDialogTop();
    this.npcDefaultLeft = this.getEasyNPC().getEasyNPCDialogData().getEntityDialogLeft();

    this.allowEscCloseValue = dialogOptions.allowEscClose();
    this.allowEscCloseCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + 10,
                this.topPos + 35,
                TextComponent.getTranslatedConfigText("dialog.allow_esc_close"),
                this.allowEscCloseValue,
                true,
                null));

    this.showCloseButtonValue = dialogOptions.showCloseButton();
    this.showCloseButtonCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + 164,
                this.topPos + 35,
                TextComponent.getTranslatedConfigText("dialog.show_close_button"),
                this.showCloseButtonValue,
                true,
                null));

    this.displayAvatarValue = dialogOptions.displayAvatar();
    this.displayAvatarCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + 10,
                this.topPos + 54,
                TextComponent.getTranslatedConfigText("dialog.display_avatar"),
                this.displayAvatarValue,
                true,
                checkbox -> this.setAvatarSlidersActive(checkbox.selected())));

    this.hideUnavailableButtonsValue =
        dialogOptions.buttonConditionMode() == DialogButtonConditionMode.HIDE;
    this.hideUnavailableButtonsCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + 164,
                this.topPos + 54,
                TextComponent.getTranslatedConfigText("dialog.hide_unavailable_buttons"),
                this.hideUnavailableButtonsValue,
                true,
                null));

    this.avatarScaleInitialValue =
        dialogOptions.hasAvatarScale() ? dialogOptions.avatarScale() : this.npcDefaultScale;
    this.avatarScaleSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                this.leftPos + 10,
                this.topPos + 87,
                SLIDER_WIDTH,
                SLIDER_HEIGHT,
                this.avatarScaleInitialValue,
                5,
                200,
                this.npcDefaultScale,
                1,
                SliderButton.Type.DOUBLE,
                true,
                sliderButton -> {}));

    // Avatar Top slider — uses POSITION type so text-edit accepts negative values
    this.avatarTopInitialValue =
        dialogOptions.hasAvatarTop() ? dialogOptions.avatarTop() : this.npcDefaultTop;
    this.avatarTopSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                this.leftPos + 10,
                this.topPos + 117,
                SLIDER_WIDTH,
                SLIDER_HEIGHT,
                this.avatarTopInitialValue,
                -200,
                200,
                this.npcDefaultTop,
                1,
                SliderButton.Type.POSITION,
                true,
                sliderButton -> {}));

    // Avatar Left slider — uses POSITION type so text-edit accepts negative values
    this.avatarLeftInitialValue =
        dialogOptions.hasAvatarLeft() ? dialogOptions.avatarLeft() : this.npcDefaultLeft;
    this.avatarLeftSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                this.leftPos + 10,
                this.topPos + 147,
                SLIDER_WIDTH,
                SLIDER_HEIGHT,
                this.avatarLeftInitialValue,
                -200,
                200,
                this.npcDefaultLeft,
                1,
                SliderButton.Type.POSITION,
                true,
                sliderButton -> {}));

    this.setAvatarSlidersActive(this.displayAvatarValue);

    this.saveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.leftPos + 8,
                this.bottomPos - 30,
                130,
                "save",
                onPress -> {
                  this.saveDialogData();
                  this.openPreviousScreen();
                }));

    this.cancelButton =
        this.addRenderableWidget(
            new CancelButton(
                this.saveButton.getX() + this.saveButton.getWidth() + 5,
                this.bottomPos - 30,
                130,
                "cancel",
                onPress -> this.openPreviousScreen()));
  }

  @Override
  public void updateTick() {
    super.updateTick();

    if (this.saveButton != null) {
      boolean avatarSlidersChanged =
          this.displayAvatarCheckbox.selected()
              && (Math.round(this.avatarScaleSlider.getTargetValue())
                      != this.avatarScaleInitialValue
                  || Math.round(this.avatarTopSlider.getTargetValue()) != this.avatarTopInitialValue
                  || Math.round(this.avatarLeftSlider.getTargetValue())
                      != this.avatarLeftInitialValue);
      this.saveButton.active =
          this.allowEscCloseCheckbox.selected() != this.allowEscCloseValue
              || this.showCloseButtonCheckbox.selected() != this.showCloseButtonValue
              || this.displayAvatarCheckbox.selected() != this.displayAvatarValue
              || this.hideUnavailableButtonsCheckbox.selected() != this.hideUnavailableButtonsValue
              || avatarSlidersChanged;
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "dialog.avatar_scale",
        this.leftPos + 10,
        this.topPos + 79,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "dialog.avatar_top",
        this.leftPos + 10,
        this.topPos + 109,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "dialog.avatar_left",
        this.leftPos + 10,
        this.topPos + 139,
        Constants.FONT_COLOR_BLACK);
  }
}
