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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.menu.configuration.action.DistanceActionConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class DistanceActionConfigurationScreen
    extends ActionConfigurationScreen<DistanceActionConfigurationMenu> {

  protected EditBox onNearActionBox;
  protected Checkbox onNearActionExecuteAsUserCheckbox;
  protected Checkbox onNearActionDebugCheckbox;
  protected Button onNearActionSaveButton;
  protected EditBox onCloseActionBox;
  protected Checkbox onCloseActionExecuteAsUserCheckbox;
  protected Checkbox onCloseActionDebugCheckbox;
  protected Button onCloseActionSaveButton;
  protected EditBox onVeryCloseActionBox;
  protected Checkbox onVeryCloseActionExecuteAsUserCheckbox;
  protected Checkbox onVeryCloseActionDebugCheckbox;
  protected Button onVeryCloseActionSaveButton;
  protected EditBox onTouchActionBox;
  protected Checkbox onTouchActionExecuteAsUserCheckbox;
  protected Checkbox onTouchActionDebugCheckbox;
  protected Button onTouchActionSaveButton;
  private ActionData lastNearActionData;
  private ActionData lastCloseActionData;
  private ActionData lastVeryCloseActionData;
  private ActionData lastTouchActionData;

  public DistanceActionConfigurationScreen(
      DistanceActionConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  public void validateNearAction() {
    ActionData actionData =
        new ActionData(
            ActionType.COMMAND,
            this.onNearActionBox.getValue(),
            this.onNearActionExecuteAsUserCheckbox.selected(),
            this.onNearActionDebugCheckbox.selected());
    this.onNearActionSaveButton.active = !actionData.equals(this.lastNearActionData);
  }

  public void validateCloseAction() {
    ActionData actionData =
        new ActionData(
            ActionType.COMMAND,
            this.onCloseActionBox.getValue(),
            this.onCloseActionExecuteAsUserCheckbox.selected(),
            this.onCloseActionDebugCheckbox.selected());
    this.onCloseActionSaveButton.active = !actionData.equals(this.lastCloseActionData);
  }

  public void validateVeryCloseAction() {
    ActionData actionData =
        new ActionData(
            ActionType.COMMAND,
            this.onVeryCloseActionBox.getValue(),
            this.onVeryCloseActionExecuteAsUserCheckbox.selected(),
            this.onVeryCloseActionDebugCheckbox.selected());
    this.onVeryCloseActionSaveButton.active = !actionData.equals(this.lastVeryCloseActionData);
  }

  public void validateTouchAction() {
    ActionData actionData =
        new ActionData(
            ActionType.COMMAND,
            this.onTouchActionBox.getValue(),
            this.onTouchActionExecuteAsUserCheckbox.selected(),
            this.onTouchActionDebugCheckbox.selected());
    this.onTouchActionSaveButton.active = !actionData.equals(this.lastTouchActionData);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.distanceActionButton.active = false;

    // On Near Distance Action
    int nearActionTop = this.topPos + 50;
    ActionData nearActionData = this.actionDataSet.getActionEvent(ActionEventType.ON_DISTANCE_NEAR);
    this.lastNearActionData = nearActionData;
    this.onNearActionBox =
        this.addRenderableWidget(actionEditBox(this.contentLeftPos, nearActionTop, nearActionData));
    this.onNearActionBox.setResponder(consumer -> this.validateNearAction());
    this.onNearActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                nearActionTop + 18,
                "execute_as_player",
                nearActionData != null && nearActionData.shouldExecuteAsUser(),
                checkbox -> this.validateNearAction()));
    this.onNearActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                nearActionTop + 18,
                "debug",
                nearActionData != null && nearActionData.isDebugEnabled(),
                checkbox -> this.validateNearAction()));
    this.onNearActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.onNearActionBox.getX() + this.onNearActionBox.getWidth() + 5,
                nearActionTop - 1,
                onPress -> {
                  ActionData actionData =
                      new ActionData(
                          ActionType.COMMAND,
                          this.onNearActionBox.getValue(),
                          this.onNearActionExecuteAsUserCheckbox.selected(),
                          this.onNearActionDebugCheckbox.selected());
                  NetworkMessageHandler.actionEventChange(
                      uuid, ActionEventType.ON_DISTANCE_NEAR, actionData);
                  this.lastNearActionData = actionData;
                  this.onNearActionSaveButton.active = false;
                }));
    this.onNearActionSaveButton.active = false;

    // On Close Distance Action
    int closeActionTop = this.topPos + 100;
    ActionData closeActionData =
        this.actionDataSet.getActionEvent(ActionEventType.ON_DISTANCE_CLOSE);
    this.lastCloseActionData = closeActionData;
    this.onCloseActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, closeActionTop, closeActionData));
    this.onCloseActionBox.setResponder(consumer -> this.validateCloseAction());
    this.onCloseActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                closeActionTop + 18,
                "execute_as_player",
                closeActionData != null && closeActionData.shouldExecuteAsUser(),
                checkbox -> this.validateCloseAction()));
    this.onCloseActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                closeActionTop + 18,
                "debug",
                closeActionData != null && closeActionData.isDebugEnabled(),
                checkbox -> this.validateCloseAction()));
    this.onCloseActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.onCloseActionBox.getX() + this.onCloseActionBox.getWidth() + 5,
                closeActionTop - 1,
                onPress -> {
                  ActionData actionData =
                      new ActionData(
                          ActionType.COMMAND,
                          this.onCloseActionBox.getValue(),
                          this.onCloseActionExecuteAsUserCheckbox.selected(),
                          this.onCloseActionDebugCheckbox.selected());
                  NetworkMessageHandler.actionEventChange(
                      uuid, ActionEventType.ON_DISTANCE_CLOSE, actionData);
                  this.lastCloseActionData = actionData;
                  this.onCloseActionSaveButton.active = false;
                }));
    this.onCloseActionSaveButton.active = false;

    // On Very Close Distance Action
    int veryCloseActionTop = this.topPos + 150;
    ActionData veryCloseActionData =
        this.actionDataSet.getActionEvent(ActionEventType.ON_DISTANCE_VERY_CLOSE);
    this.lastVeryCloseActionData = veryCloseActionData;
    this.onVeryCloseActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, veryCloseActionTop, veryCloseActionData));
    this.onVeryCloseActionBox.setResponder(consumer -> this.validateVeryCloseAction());
    this.onVeryCloseActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                veryCloseActionTop + 18,
                "execute_as_player",
                veryCloseActionData != null && veryCloseActionData.shouldExecuteAsUser(),
                checkbox -> this.validateVeryCloseAction()));
    this.onVeryCloseActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                veryCloseActionTop + 18,
                "debug",
                veryCloseActionData != null && veryCloseActionData.isDebugEnabled(),
                checkbox -> this.validateVeryCloseAction()));
    this.onVeryCloseActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.onVeryCloseActionBox.getX() + this.onVeryCloseActionBox.getWidth() + 5,
                veryCloseActionTop - 1,
                onPress -> {
                  ActionData actionData =
                      new ActionData(
                          ActionType.COMMAND,
                          this.onVeryCloseActionBox.getValue(),
                          this.onVeryCloseActionExecuteAsUserCheckbox.selected(),
                          this.onVeryCloseActionDebugCheckbox.selected());
                  NetworkMessageHandler.actionEventChange(
                      uuid, ActionEventType.ON_DISTANCE_VERY_CLOSE, actionData);
                  this.lastVeryCloseActionData = actionData;
                  this.onVeryCloseActionSaveButton.active = false;
                }));
    this.onVeryCloseActionSaveButton.active = false;

    // On Touch Distance Action
    int touchActionTop = this.topPos + 200;
    ActionData touchActionData =
        this.actionDataSet.getActionEvent(ActionEventType.ON_DISTANCE_TOUCH);
    this.lastTouchActionData = touchActionData;
    this.onTouchActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, touchActionTop, touchActionData));
    this.onTouchActionBox.setResponder(consumer -> this.validateTouchAction());
    this.onTouchActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                touchActionTop + 18,
                "execute_as_player",
                touchActionData != null && touchActionData.shouldExecuteAsUser(),
                checkbox -> this.validateTouchAction()));
    this.onTouchActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                touchActionTop + 18,
                "debug",
                touchActionData != null && touchActionData.isDebugEnabled(),
                checkbox -> this.validateTouchAction()));
    this.onTouchActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.onTouchActionBox.getX() + this.onTouchActionBox.getWidth() + 5,
                touchActionTop - 1,
                onPress -> {
                  ActionData actionData =
                      new ActionData(
                          ActionType.COMMAND,
                          this.onTouchActionBox.getValue(),
                          this.onTouchActionExecuteAsUserCheckbox.selected(),
                          this.onTouchActionDebugCheckbox.selected());
                  NetworkMessageHandler.actionEventChange(
                      uuid, ActionEventType.ON_DISTANCE_TOUCH, actionData);
                  this.lastTouchActionData = actionData;
                  this.onTouchActionSaveButton.active = false;
                }));
    this.onTouchActionSaveButton.active = false;
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Description Texts
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "on_distance_near",
        this.contentLeftPos,
        this.onNearActionSaveButton.getY() - 10,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "on_distance_close",
        this.contentLeftPos,
        this.onCloseActionSaveButton.getY() - 10,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "on_distance_very_close",
        this.contentLeftPos,
        this.onVeryCloseActionSaveButton.getY() - 10,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "on_distance_touch",
        this.contentLeftPos,
        this.onTouchActionSaveButton.getY() - 10,
        Constants.FONT_COLOR_BLACK);
  }
}
