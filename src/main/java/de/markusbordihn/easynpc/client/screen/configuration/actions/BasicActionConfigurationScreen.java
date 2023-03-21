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

package de.markusbordihn.easynpc.client.screen.configuration.actions;

import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.action.ActionData;
import de.markusbordihn.easynpc.action.ActionType;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.menu.configuration.action.BasicActionConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessage;

@OnlyIn(Dist.CLIENT)
public class BasicActionConfigurationScreen
    extends ActionConfigurationScreen<BasicActionConfigurationMenu> {

  // On Interaction Action
  protected EditBox onInteractionActionBox;
  protected Checkbox onInteractionActionExecuteAsUserCheckbox;
  protected Checkbox onInteractionActionDebugCheckbox;
  protected Button onInteractionActionSaveButton;

  // Cache
  private ActionData lastInteractionActionData;

  public BasicActionConfigurationScreen(BasicActionConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  public void validateInteractionAction() {
    ActionData actionData =
        new ActionData(ActionType.ON_INTERACTION, this.onInteractionActionBox.getValue(),
            this.onInteractionActionExecuteAsUserCheckbox.selected(),
            this.onInteractionActionDebugCheckbox.selected());
    this.onInteractionActionSaveButton.active = !actionData.equals(this.lastInteractionActionData);
  }

  @Override
  public void init() {
    super.init();

    // On Interaction Action
    int interactionActionTop = this.topPos + 50;
    ActionData interactionActionData = this.entity.getActionData(ActionType.ON_INTERACTION);
    this.lastInteractionActionData = interactionActionData;
    this.onInteractionActionBox = this.addRenderableWidget(
        actionEditBox(this.contentLeftPos, interactionActionTop, interactionActionData));
    this.onInteractionActionBox.setResponder(consumer -> this.validateInteractionAction());
    this.onInteractionActionExecuteAsUserCheckbox = this.addRenderableWidget(
        new Checkbox(this.contentLeftPos + 80, interactionActionTop + 18, "execute_as_player",
            interactionActionData != null && interactionActionData.shouldExecuteAsUser(),
            checkbox -> this.validateInteractionAction()));
    this.onInteractionActionDebugCheckbox =
        this.addRenderableWidget(new Checkbox(this.contentLeftPos + 215, interactionActionTop + 18,
            "debug", interactionActionData != null && interactionActionData.isDebugEnabled(),
            checkbox -> this.validateInteractionAction()));
    this.onInteractionActionSaveButton = this.addRenderableWidget(
        new SaveButton(this.leftPos + 267, interactionActionTop - 2, onPress -> {
          ActionData actionData =
              new ActionData(ActionType.ON_INTERACTION, this.onInteractionActionBox.getValue(),
                  this.onInteractionActionExecuteAsUserCheckbox.selected(),
                  this.onInteractionActionDebugCheckbox.selected());
          NetworkMessage.actionChange(uuid, actionData);
          this.lastInteractionActionData = actionData;
          this.onInteractionActionSaveButton.active = false;
        }));
    this.onInteractionActionSaveButton.active = false;

    // Default button stats
    this.basicActionButton.active = false;
    this.dialogActionButton.active = true;
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Description Texts
    this.font.draw(poseStack,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "on_interaction"),
        this.contentLeftPos, this.onInteractionActionSaveButton.getY() - 8f,
        Constants.FONT_COLOR_BLACK);

  }

}
