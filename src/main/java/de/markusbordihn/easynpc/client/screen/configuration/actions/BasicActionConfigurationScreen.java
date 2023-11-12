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
import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.menu.configuration.action.BasicActionConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class BasicActionConfigurationScreen
    extends ActionConfigurationScreen<BasicActionConfigurationMenu> {

  // On Interaction Action
  protected EditBox onInteractionActionBox;
  protected Checkbox onInteractionActionExecuteAsUserCheckbox;
  protected Checkbox onInteractionActionDebugCheckbox;
  protected Button onInteractionActionSaveButton;

  // On Hurt Action
  protected EditBox onHurtActionBox;
  protected Checkbox onHurtActionExecuteAsUserCheckbox;
  protected Checkbox onHurtActionDebugCheckbox;
  protected Button onHurtActionSaveButton;

  // On Death Action
  protected EditBox onDeathActionBox;
  protected Checkbox onDeathActionExecuteAsUserCheckbox;
  protected Checkbox onDeathActionDebugCheckbox;
  protected Button onDeathActionSaveButton;

  // Cache
  private ActionData lastInteractionActionData;
  private ActionData lastHurtActionData;
  private ActionData lastDeathActionData;

  public BasicActionConfigurationScreen(
      BasicActionConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  public void validateInteractionAction() {
    ActionData actionData =
        new ActionData(
            ActionType.COMMAND,
            this.onInteractionActionBox.getValue(),
            this.onInteractionActionExecuteAsUserCheckbox.selected(),
            this.onInteractionActionDebugCheckbox.selected());
    this.onInteractionActionSaveButton.active = !actionData.equals(this.lastInteractionActionData);
  }

  public void validateOnHurtAction() {
    ActionData actionData =
        new ActionData(
            ActionType.COMMAND,
            this.onHurtActionBox.getValue(),
            this.onHurtActionExecuteAsUserCheckbox.selected(),
            this.onHurtActionDebugCheckbox.selected());
    this.onHurtActionSaveButton.active = !actionData.equals(this.lastHurtActionData);
  }

  public void validateOnDeathAction() {
    ActionData actionData =
        new ActionData(
            ActionType.COMMAND,
            this.onDeathActionBox.getValue(),
            this.onDeathActionExecuteAsUserCheckbox.selected(),
            this.onDeathActionDebugCheckbox.selected());
    this.onDeathActionSaveButton.active = !actionData.equals(this.lastDeathActionData);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.basicActionButton.active = false;

    // On Interaction Action
    int interactionActionTop = this.topPos + 50;
    ActionData interactionActionData =
        this.actionDataSet.getActionEvent(ActionEventType.ON_INTERACTION);
    this.lastInteractionActionData = interactionActionData;
    this.onInteractionActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, interactionActionTop, interactionActionData));
    this.onInteractionActionBox.setResponder(consumer -> this.validateInteractionAction());
    this.onInteractionActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                interactionActionTop + 18,
                "execute_as_player",
                interactionActionData != null && interactionActionData.shouldExecuteAsUser(),
                checkbox -> this.validateInteractionAction()));
    this.onInteractionActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                interactionActionTop + 18,
                "debug",
                interactionActionData != null && interactionActionData.isDebugEnabled(),
                checkbox -> this.validateInteractionAction()));
    this.onInteractionActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                onInteractionActionBox.x + onInteractionActionBox.getWidth() + 5,
                interactionActionTop - 1,
                onPress -> {
                  ActionData actionData =
                      new ActionData(
                          ActionType.COMMAND,
                          this.onInteractionActionBox.getValue(),
                          this.onInteractionActionExecuteAsUserCheckbox.selected(),
                          this.onInteractionActionDebugCheckbox.selected());
                  NetworkMessageHandler.actionEventChange(
                      uuid, ActionEventType.ON_INTERACTION, actionData);
                  this.lastInteractionActionData = actionData;
                  this.onInteractionActionSaveButton.active = false;
                }));
    this.onInteractionActionSaveButton.active = false;

    // On Hurt Action
    int onHurtActionTop = interactionActionTop + 50;
    ActionData onHurtActionData = this.actionDataSet.getActionEvent(ActionEventType.ON_HURT);
    this.lastHurtActionData = onHurtActionData;
    this.onHurtActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, onHurtActionTop, onHurtActionData));
    this.onHurtActionBox.setResponder(consumer -> this.validateOnHurtAction());
    this.onHurtActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                onHurtActionTop + 18,
                "execute_as_player",
                onHurtActionData != null && onHurtActionData.shouldExecuteAsUser(),
                checkbox -> this.validateOnHurtAction()));
    this.onHurtActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                onHurtActionTop + 18,
                "debug",
                onHurtActionData != null && onHurtActionData.isDebugEnabled(),
                checkbox -> this.validateOnHurtAction()));
    this.onHurtActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                onHurtActionBox.x + onHurtActionBox.getWidth() + 5,
                onHurtActionTop - 1,
                onPress -> {
                  ActionData actionData =
                      new ActionData(
                          ActionType.COMMAND,
                          this.onHurtActionBox.getValue(),
                          this.onHurtActionExecuteAsUserCheckbox.selected(),
                          this.onHurtActionDebugCheckbox.selected());
                  NetworkMessageHandler.actionEventChange(
                      uuid, ActionEventType.ON_HURT, actionData);
                  this.lastHurtActionData = actionData;
                  this.onHurtActionSaveButton.active = false;
                }));
    this.onHurtActionSaveButton.active = false;

    // On Death Action
    int onDeathActionTop = onHurtActionTop + 50;
    ActionData onDeathActionData = this.actionDataSet.getActionEvent(ActionEventType.ON_DEATH);
    this.lastDeathActionData = onDeathActionData;
    this.onDeathActionBox =
        this.addRenderableWidget(
            actionEditBox(this.contentLeftPos, onDeathActionTop, onDeathActionData));
    this.onDeathActionBox.setResponder(consumer -> this.validateOnDeathAction());
    this.onDeathActionExecuteAsUserCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 80,
                onDeathActionTop + 18,
                "execute_as_player",
                onDeathActionData != null && onDeathActionData.shouldExecuteAsUser(),
                checkbox -> this.validateOnDeathAction()));
    this.onDeathActionDebugCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 215,
                onDeathActionTop + 18,
                "debug",
                onDeathActionData != null && onDeathActionData.isDebugEnabled(),
                checkbox -> this.validateOnDeathAction()));
    this.onDeathActionSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                onDeathActionBox.x + onDeathActionBox.getWidth() + 5,
                onDeathActionTop - 1,
                onPress -> {
                  ActionData actionData =
                      new ActionData(
                          ActionType.COMMAND,
                          this.onDeathActionBox.getValue(),
                          this.onDeathActionExecuteAsUserCheckbox.selected(),
                          this.onDeathActionDebugCheckbox.selected());
                  NetworkMessageHandler.actionEventChange(
                      uuid, ActionEventType.ON_DEATH, actionData);
                  this.lastDeathActionData = actionData;
                  this.onDeathActionSaveButton.active = false;
                }));
    this.onDeathActionSaveButton.active = false;
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Description Texts
    Text.drawConfigString(
        poseStack,
        this.font,
        "on_interaction",
        this.contentLeftPos,
        this.onInteractionActionSaveButton.y - 10,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        poseStack,
        this.font,
        "on_hurt",
        this.contentLeftPos,
        this.onHurtActionSaveButton.y - 10,
        Constants.FONT_COLOR_BLACK);

    Text.drawConfigString(
        poseStack,
        this.font,
        "on_death",
        this.contentLeftPos,
        this.onDeathActionSaveButton.y - 10,
        Constants.FONT_COLOR_BLACK);
  }
}
