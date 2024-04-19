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
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.menu.configuration.action.BasicActionConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class BasicActionConfigurationScreen
    extends ActionConfigurationScreen<BasicActionConfigurationMenu> {

  protected EditBox onInteractionActionBox;
  protected Checkbox onInteractionActionExecuteAsUserCheckbox;
  protected Checkbox onInteractionActionDebugCheckbox;
  protected Button onInteractionActionSaveButton;
  protected EditBox onHurtActionBox;
  protected Checkbox onHurtActionExecuteAsUserCheckbox;
  protected Checkbox onHurtActionDebugCheckbox;
  protected Button onHurtActionSaveButton;
  protected EditBox onDeathActionBox;
  protected Checkbox onDeathActionExecuteAsUserCheckbox;
  protected Checkbox onDeathActionDebugCheckbox;
  protected Button onDeathActionSaveButton;
  private ActionDataEntry lastInteractionActionDataEntry;
  private ActionDataEntry lastHurtActionDataEntry;
  private ActionDataEntry lastDeathActionDataEntry;

  public BasicActionConfigurationScreen(
      BasicActionConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  public void validateInteractionAction() {
    ActionDataEntry actionDataEntry =
        new ActionDataEntry(
            ActionType.COMMAND,
            this.onInteractionActionBox.getValue(),
            this.onInteractionActionExecuteAsUserCheckbox.selected(),
            this.onInteractionActionDebugCheckbox.selected());
    this.onInteractionActionSaveButton.active =
        !actionDataEntry.equals(this.lastInteractionActionDataEntry);
  }

  public void validateOnHurtAction() {
    ActionDataEntry actionDataEntry =
        new ActionDataEntry(
            ActionType.COMMAND,
            this.onHurtActionBox.getValue(),
            this.onHurtActionExecuteAsUserCheckbox.selected(),
            this.onHurtActionDebugCheckbox.selected());
    this.onHurtActionSaveButton.active = !actionDataEntry.equals(this.lastHurtActionDataEntry);
  }

  public void validateOnDeathAction() {
    ActionDataEntry actionDataEntry =
        new ActionDataEntry(
            ActionType.COMMAND,
            this.onDeathActionBox.getValue(),
            this.onDeathActionExecuteAsUserCheckbox.selected(),
            this.onDeathActionDebugCheckbox.selected());
    this.onDeathActionSaveButton.active = !actionDataEntry.equals(this.lastDeathActionDataEntry);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.basicActionButton.active = false;

    // On Interaction Action
    int interactionActionTop = this.topPos + 50;
    ActionDataEntry interactionActionDataEntry =
        this.actionDataSet.getActionEvent(ActionEventType.ON_INTERACTION);
    this.lastInteractionActionDataEntry = interactionActionDataEntry;
    this.onInteractionActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, interactionActionTop, interactionActionDataEntry));
    this.onInteractionActionBox.setResponder(consumer -> this.validateInteractionAction());
    this.onInteractionActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                interactionActionTop + 18,
                "execute_as_player",
                interactionActionDataEntry != null
                    && interactionActionDataEntry.shouldExecuteAsUser(),
                checkbox -> this.validateInteractionAction()));
    this.onInteractionActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                interactionActionTop + 18,
                "debug",
                interactionActionDataEntry != null && interactionActionDataEntry.isDebugEnabled(),
                checkbox -> this.validateInteractionAction()));
    this.onInteractionActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                onInteractionActionBox.getX() + onInteractionActionBox.getWidth() + 5,
                interactionActionTop - 1,
                onPress -> {
                  ActionDataEntry actionDataEntry =
                      new ActionDataEntry(
                          ActionType.COMMAND,
                          this.onInteractionActionBox.getValue(),
                          this.onInteractionActionExecuteAsUserCheckbox.selected(),
                          this.onInteractionActionDebugCheckbox.selected());
                  NetworkMessageHandler.actionEventChange(
                      uuid, ActionEventType.ON_INTERACTION, actionDataEntry);
                  this.lastInteractionActionDataEntry = actionDataEntry;
                  this.onInteractionActionSaveButton.active = false;
                }));
    this.onInteractionActionSaveButton.active = false;

    // On Hurt Action
    int onHurtActionTop = interactionActionTop + 50;
    ActionDataEntry onHurtActionDataEntry =
        this.actionDataSet.getActionEvent(ActionEventType.ON_HURT);
    this.lastHurtActionDataEntry = onHurtActionDataEntry;
    this.onHurtActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, onHurtActionTop, onHurtActionDataEntry));
    this.onHurtActionBox.setResponder(consumer -> this.validateOnHurtAction());
    this.onHurtActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                onHurtActionTop + 18,
                "execute_as_player",
                onHurtActionDataEntry != null && onHurtActionDataEntry.shouldExecuteAsUser(),
                checkbox -> this.validateOnHurtAction()));
    this.onHurtActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                onHurtActionTop + 18,
                "debug",
                onHurtActionDataEntry != null && onHurtActionDataEntry.isDebugEnabled(),
                checkbox -> this.validateOnHurtAction()));
    this.onHurtActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                onHurtActionBox.getX() + onHurtActionBox.getWidth() + 5,
                onHurtActionTop - 1,
                onPress -> {
                  ActionDataEntry actionDataEntry =
                      new ActionDataEntry(
                          ActionType.COMMAND,
                          this.onHurtActionBox.getValue(),
                          this.onHurtActionExecuteAsUserCheckbox.selected(),
                          this.onHurtActionDebugCheckbox.selected());
                  NetworkMessageHandler.actionEventChange(
                      uuid, ActionEventType.ON_HURT, actionDataEntry);
                  this.lastHurtActionDataEntry = actionDataEntry;
                  this.onHurtActionSaveButton.active = false;
                }));
    this.onHurtActionSaveButton.active = false;

    // On Death Action
    int onDeathActionTop = onHurtActionTop + 50;
    ActionDataEntry onDeathActionDataEntry =
        this.actionDataSet.getActionEvent(ActionEventType.ON_DEATH);
    this.lastDeathActionDataEntry = onDeathActionDataEntry;
    this.onDeathActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, onDeathActionTop, onDeathActionDataEntry));
    this.onDeathActionBox.setResponder(consumer -> this.validateOnDeathAction());
    this.onDeathActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                onDeathActionTop + 18,
                "execute_as_player",
                onDeathActionDataEntry != null && onDeathActionDataEntry.shouldExecuteAsUser(),
                checkbox -> this.validateOnDeathAction()));
    this.onDeathActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                onDeathActionTop + 18,
                "debug",
                onDeathActionDataEntry != null && onDeathActionDataEntry.isDebugEnabled(),
                checkbox -> this.validateOnDeathAction()));
    this.onDeathActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                onDeathActionBox.getX() + onDeathActionBox.getWidth() + 5,
                onDeathActionTop - 1,
                onPress -> {
                  ActionDataEntry actionDataEntry =
                      new ActionDataEntry(
                          ActionType.COMMAND,
                          this.onDeathActionBox.getValue(),
                          this.onDeathActionExecuteAsUserCheckbox.selected(),
                          this.onDeathActionDebugCheckbox.selected());
                  NetworkMessageHandler.actionEventChange(
                      uuid, ActionEventType.ON_DEATH, actionDataEntry);
                  this.lastDeathActionDataEntry = actionDataEntry;
                  this.onDeathActionSaveButton.active = false;
                }));
    this.onDeathActionSaveButton.active = false;
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Description Texts
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "on_interaction",
        this.contentLeftPos,
        this.onInteractionActionSaveButton.getY() - 10,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "on_hurt",
        this.contentLeftPos,
        this.onHurtActionSaveButton.getY() - 10,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "on_death",
        this.contentLeftPos,
        this.onDeathActionSaveButton.getY() - 10,
        Constants.FONT_COLOR_BLACK);
  }
}
