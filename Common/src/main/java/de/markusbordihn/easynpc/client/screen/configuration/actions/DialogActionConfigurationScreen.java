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

package de.markusbordihn.easynpc.client.screen.configuration.actions;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class DialogActionConfigurationScreen<T extends ConfigurationMenu>
    extends ActionConfigurationScreen<T> {

  protected EditBox onOpenDialogActionBox;
  protected Checkbox onOpenDialogActionExecuteAsUserCheckbox;
  protected Checkbox onOpenDialogActionDebugCheckbox;
  protected Button onOpenDialogActionSaveButton;
  protected EditBox onCloseDialogActionBox;
  protected Checkbox onCloseDialogActionExecuteAsUserCheckbox;
  protected Checkbox onCloseDialogActionDebugCheckbox;
  protected Button onCloseDialogActionSaveButton;
  protected EditBox onButtonClickActionBox;
  protected Checkbox onButtonClickActionExecuteAsUserCheckbox;
  protected Checkbox onButtonClickActionDebugCheckbox;
  protected Button onButtonClickActionSaveButton;
  private ActionDataEntry lastButtonClickActionDataEntry;
  private ActionDataEntry lastCloseDialogActionDataEntry;
  private ActionDataEntry lastOpenDialogActionDataEntry;

  public DialogActionConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  public void validateOpenDialogAction() {
    ActionDataEntry actionDataEntry =
        new ActionDataEntry(
            ActionType.COMMAND,
            this.onOpenDialogActionBox.getValue(),
            this.onOpenDialogActionExecuteAsUserCheckbox.selected(),
            this.onOpenDialogActionDebugCheckbox.selected());
    this.onOpenDialogActionSaveButton.active =
        !actionDataEntry.equals(this.lastOpenDialogActionDataEntry);
  }

  public void validateCloseDialogAction() {
    ActionDataEntry actionDataEntry =
        new ActionDataEntry(
            ActionType.COMMAND,
            this.onCloseDialogActionBox.getValue(),
            this.onCloseDialogActionExecuteAsUserCheckbox.selected(),
            this.onCloseDialogActionDebugCheckbox.selected());
    this.onCloseDialogActionSaveButton.active =
        !actionDataEntry.equals(this.lastCloseDialogActionDataEntry);
  }

  public void validateButtonClickAction() {
    ActionDataEntry actionDataEntry =
        new ActionDataEntry(
            ActionType.COMMAND,
            this.onButtonClickActionBox.getValue(),
            this.onButtonClickActionExecuteAsUserCheckbox.selected(),
            this.onButtonClickActionDebugCheckbox.selected());
    this.onButtonClickActionSaveButton.active =
        !actionDataEntry.equals(this.lastButtonClickActionDataEntry);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.dialogActionButton.active = false;

    // On Open Dialog Action
    int openDialogActionTop = this.topPos + 50;
    ActionDataEntry openDialogActionDataEntry =
        this.getActionEventSet().getActionEvent(ActionEventType.ON_OPEN_DIALOG);
    this.lastOpenDialogActionDataEntry = openDialogActionDataEntry;
    this.onOpenDialogActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, openDialogActionTop, openDialogActionDataEntry));
    this.onOpenDialogActionBox.setResponder(consumer -> this.validateOpenDialogAction());
    this.onOpenDialogActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                openDialogActionTop + 18,
                "execute_as_player",
                openDialogActionDataEntry != null
                    && openDialogActionDataEntry.shouldExecuteAsUser(),
                checkbox -> this.validateOpenDialogAction()));
    this.onOpenDialogActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                openDialogActionTop + 18,
                "debug",
                openDialogActionDataEntry != null && openDialogActionDataEntry.isDebugEnabled(),
                checkbox -> this.validateOpenDialogAction()));
    this.onOpenDialogActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.onOpenDialogActionBox.x + this.onOpenDialogActionBox.getWidth() + 5,
                openDialogActionTop - 1,
                onPress -> {
                  ActionDataEntry actionDataEntry =
                      new ActionDataEntry(
                          ActionType.COMMAND,
                          this.onOpenDialogActionBox.getValue(),
                          this.onOpenDialogActionExecuteAsUserCheckbox.selected(),
                          this.onOpenDialogActionDebugCheckbox.selected());
                  NetworkMessageHandlerManager.getServerHandler()
                      .actionEventChange(
                          this.getNpcUUID(), ActionEventType.ON_OPEN_DIALOG, actionDataEntry);
                  this.lastOpenDialogActionDataEntry = actionDataEntry;
                  this.onOpenDialogActionSaveButton.active = false;
                }));
    this.onOpenDialogActionSaveButton.active = false;

    // On Close Dialog Action
    int closeDialogActionTop = this.topPos + 100;
    ActionDataEntry closeDialogActionDataEntry =
        this.getActionEventSet().getActionEvent(ActionEventType.ON_CLOSE_DIALOG);
    this.lastCloseDialogActionDataEntry = closeDialogActionDataEntry;
    this.onCloseDialogActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, closeDialogActionTop, closeDialogActionDataEntry));
    this.onCloseDialogActionBox.setResponder(consumer -> this.validateCloseDialogAction());
    this.onCloseDialogActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                closeDialogActionTop + 18,
                "execute_as_player",
                closeDialogActionDataEntry != null
                    && closeDialogActionDataEntry.shouldExecuteAsUser(),
                checkbox -> this.validateCloseDialogAction()));
    this.onCloseDialogActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                closeDialogActionTop + 18,
                "debug",
                closeDialogActionDataEntry != null && closeDialogActionDataEntry.isDebugEnabled(),
                checkbox -> this.validateCloseDialogAction()));
    this.onCloseDialogActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.onCloseDialogActionBox.x + this.onCloseDialogActionBox.getWidth() + 5,
                closeDialogActionTop - 1,
                onPress -> {
                  ActionDataEntry actionDataEntry =
                      new ActionDataEntry(
                          ActionType.COMMAND,
                          this.onCloseDialogActionBox.getValue(),
                          this.onCloseDialogActionExecuteAsUserCheckbox.selected(),
                          this.onCloseDialogActionDebugCheckbox.selected());
                  NetworkMessageHandlerManager.getServerHandler()
                      .actionEventChange(
                          this.getNpcUUID(), ActionEventType.ON_CLOSE_DIALOG, actionDataEntry);
                  this.lastCloseDialogActionDataEntry = actionDataEntry;
                  this.onCloseDialogActionSaveButton.active = false;
                }));
    this.onCloseDialogActionSaveButton.active = false;

    // On Yes Selection Action
    int onButtonClickActionTop = this.topPos + 150;
    ActionDataEntry onButtonClickActionDataEntry =
        this.getActionEventSet().getActionEvent(ActionEventType.ON_BUTTON_CLICK);
    this.lastButtonClickActionDataEntry = onButtonClickActionDataEntry;
    this.onButtonClickActionBox =
        this.addRenderableWidget(
            actionEditBox(
                this.contentLeftPos, onButtonClickActionTop, onButtonClickActionDataEntry));
    this.onButtonClickActionBox.setResponder(consumer -> this.validateButtonClickAction());
    this.onButtonClickActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                onButtonClickActionTop + 18,
                "execute_as_player",
                onButtonClickActionDataEntry != null
                    && onButtonClickActionDataEntry.shouldExecuteAsUser(),
                checkbox -> this.validateButtonClickAction()));
    this.onButtonClickActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                onButtonClickActionTop + 18,
                "debug",
                onButtonClickActionDataEntry != null
                    && onButtonClickActionDataEntry.isDebugEnabled(),
                checkbox -> this.validateButtonClickAction()));
    this.onButtonClickActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.onButtonClickActionBox.x + this.onButtonClickActionBox.getWidth() + 5,
                onButtonClickActionTop - 1,
                onPress -> {
                  ActionDataEntry actionDataEntry =
                      new ActionDataEntry(
                          ActionType.COMMAND,
                          this.onButtonClickActionBox.getValue(),
                          this.onButtonClickActionExecuteAsUserCheckbox.selected(),
                          this.onButtonClickActionDebugCheckbox.selected());
                  NetworkMessageHandlerManager.getServerHandler()
                      .actionEventChange(
                          this.getNpcUUID(), ActionEventType.ON_BUTTON_CLICK, actionDataEntry);
                  this.lastButtonClickActionDataEntry = actionDataEntry;
                  this.onButtonClickActionSaveButton.active = false;
                }));
    this.onButtonClickActionSaveButton.active = false;
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Description Texts
    if (this.onOpenDialogActionSaveButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "on_open_dialog",
          this.contentLeftPos,
          this.onOpenDialogActionSaveButton.y - 10,
          Constants.FONT_COLOR_BLACK);
    }
    if (this.onCloseDialogActionSaveButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "on_close_dialog",
          this.contentLeftPos,
          this.onCloseDialogActionSaveButton.y - 10,
          Constants.FONT_COLOR_BLACK);
    }
    if (this.onButtonClickActionSaveButton != null) {
      Text.drawConfigString(
          poseStack,
          this.font,
          "on_button_click",
          this.contentLeftPos,
          this.onButtonClickActionSaveButton.y - 10,
          Constants.FONT_COLOR_BLACK);
    }
  }
}
