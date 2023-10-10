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

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.menu.configuration.action.DialogActionConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;

@OnlyIn(Dist.CLIENT)
public class DialogActionConfigurationScreen
    extends ActionConfigurationScreen<DialogActionConfigurationMenu> {

  // On Open Dialog Action
  protected EditBox onOpenDialogActionBox;
  protected Checkbox onOpenDialogActionExecuteAsUserCheckbox;
  protected Checkbox onOpenDialogActionDebugCheckbox;
  protected Button onOpenDialogActionSaveButton;

  // On Close Dialog Action
  protected EditBox onCloseDialogActionBox;
  protected Checkbox onCloseDialogActionExecuteAsUserCheckbox;
  protected Checkbox onCloseDialogActionDebugCheckbox;
  protected Button onCloseDialogActionSaveButton;

  // On Yes Selection Action
  protected EditBox onYesSelectionActionBox;
  protected Checkbox onYesSelectionActionExecuteAsUserCheckbox;
  protected Checkbox onYesSelectionActionDebugCheckbox;
  protected Button onYesSelectionActionSaveButton;

  // On No Selection Action
  protected EditBox onNoSelectionActionBox;
  protected Checkbox onNoSelectionActionExecuteAsUserCheckbox;
  protected Checkbox onNoSelectionActionDebugCheckbox;
  protected Button onNoSelectionActionSaveButton;

  // Cache
  private ActionData lastOpenDialogActionData;
  private ActionData lastCloseDialogActionData;
  private ActionData lastYesSelectionActionData;
  private ActionData lastNoSelectionActionData;

  public DialogActionConfigurationScreen(DialogActionConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  public void validateOpenDialogAction() {
    ActionData actionData =
        new ActionData(ActionType.ON_OPEN_DIALOG, this.onOpenDialogActionBox.getValue(),
            this.onOpenDialogActionExecuteAsUserCheckbox.selected(),
            this.onOpenDialogActionDebugCheckbox.selected());
    this.onOpenDialogActionSaveButton.active = !actionData.equals(this.lastOpenDialogActionData);
  }

  public void validateCloseDialogAction() {
    ActionData actionData =
        new ActionData(ActionType.ON_CLOSE_DIALOG, this.onCloseDialogActionBox.getValue(),
            this.onCloseDialogActionExecuteAsUserCheckbox.selected(),
            this.onCloseDialogActionDebugCheckbox.selected());
    this.onCloseDialogActionSaveButton.active = !actionData.equals(this.lastCloseDialogActionData);
  }

  public void validateYesSelectionAction() {
    ActionData actionData =
        new ActionData(ActionType.ON_YES_SELECTION, this.onYesSelectionActionBox.getValue(),
            this.onYesSelectionActionExecuteAsUserCheckbox.selected(),
            this.onYesSelectionActionDebugCheckbox.selected());
    this.onYesSelectionActionSaveButton.active =
        !actionData.equals(this.lastYesSelectionActionData);
  }

  public void validateNoSelectionAction() {
    ActionData actionData =
        new ActionData(ActionType.ON_NO_SELECTION, this.onNoSelectionActionBox.getValue(),
            this.onNoSelectionActionExecuteAsUserCheckbox.selected(),
            this.onNoSelectionActionDebugCheckbox.selected());
    this.onNoSelectionActionSaveButton.active = !actionData.equals(this.lastNoSelectionActionData);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.dialogActionButton.active = false;

    // On Open Dialog Action
    int openDialogActionTop = this.topPos + 50;
    ActionData openDialogActionData = this.entity.getActionData(ActionType.ON_OPEN_DIALOG);
    this.lastOpenDialogActionData = openDialogActionData;
    this.onOpenDialogActionBox = this.addRenderableWidget(
        actionEditBox(this.contentLeftPos, openDialogActionTop, openDialogActionData));
    this.onOpenDialogActionBox.setResponder(consumer -> this.validateOpenDialogAction());
    this.onOpenDialogActionExecuteAsUserCheckbox = this.addRenderableWidget(
        new Checkbox(this.contentLeftPos + 80, openDialogActionTop + 18, "execute_as_player",
            openDialogActionData != null && openDialogActionData.shouldExecuteAsUser(),
            checkbox -> this.validateOpenDialogAction()));
    this.onOpenDialogActionDebugCheckbox =
        this.addRenderableWidget(new Checkbox(this.contentLeftPos + 215, openDialogActionTop + 18,
            "debug", openDialogActionData != null && openDialogActionData.isDebugEnabled(),
            checkbox -> this.validateOpenDialogAction()));
    this.onOpenDialogActionSaveButton = this.addRenderableWidget(
        new SaveButton(this.onOpenDialogActionBox.x + this.onOpenDialogActionBox.getWidth() + 5,
            openDialogActionTop - 2, onPress -> {
              ActionData actionData =
                  new ActionData(ActionType.ON_OPEN_DIALOG, this.onOpenDialogActionBox.getValue(),
                      this.onOpenDialogActionExecuteAsUserCheckbox.selected(),
                      this.onOpenDialogActionDebugCheckbox.selected());
              NetworkMessageHandler.actionChange(uuid, actionData);
              this.lastOpenDialogActionData = actionData;
              this.onOpenDialogActionSaveButton.active = false;
            }));
    this.onOpenDialogActionSaveButton.active = false;

    // On Close Dialog Action
    int closeDialogActionTop = this.topPos + 100;
    ActionData closeDialogActionData = this.entity.getActionData(ActionType.ON_CLOSE_DIALOG);
    this.lastCloseDialogActionData = closeDialogActionData;
    this.onCloseDialogActionBox = this.addRenderableWidget(
        actionEditBox(this.contentLeftPos, closeDialogActionTop, closeDialogActionData));
    this.onCloseDialogActionBox.setResponder(consumer -> this.validateCloseDialogAction());
    this.onCloseDialogActionExecuteAsUserCheckbox = this.addRenderableWidget(
        new Checkbox(this.contentLeftPos + 80, closeDialogActionTop + 18, "execute_as_player",
            closeDialogActionData != null && closeDialogActionData.shouldExecuteAsUser(),
            checkbox -> this.validateCloseDialogAction()));
    this.onCloseDialogActionDebugCheckbox =
        this.addRenderableWidget(new Checkbox(this.contentLeftPos + 215, closeDialogActionTop + 18,
            "debug", closeDialogActionData != null && closeDialogActionData.isDebugEnabled(),
            checkbox -> this.validateCloseDialogAction()));
    this.onCloseDialogActionSaveButton = this.addRenderableWidget(
        new SaveButton(this.onCloseDialogActionBox.x + this.onCloseDialogActionBox.getWidth() + 5,
            closeDialogActionTop - 2, onPress -> {
              ActionData actionData =
                  new ActionData(ActionType.ON_CLOSE_DIALOG, this.onCloseDialogActionBox.getValue(),
                      this.onCloseDialogActionExecuteAsUserCheckbox.selected(),
                      this.onCloseDialogActionDebugCheckbox.selected());
              NetworkMessageHandler.actionChange(uuid, actionData);
              this.lastCloseDialogActionData = actionData;
              this.onCloseDialogActionSaveButton.active = false;
            }));
    this.onCloseDialogActionSaveButton.active = false;

    // On Yes Selection Action
    int yesSelectionActionTop = this.topPos + 150;
    ActionData yesSelectionActionData = this.entity.getActionData(ActionType.ON_YES_SELECTION);
    this.lastYesSelectionActionData = yesSelectionActionData;
    this.onYesSelectionActionBox = this.addRenderableWidget(
        actionEditBox(this.contentLeftPos, yesSelectionActionTop, yesSelectionActionData));
    this.onYesSelectionActionBox.setResponder(consumer -> this.validateYesSelectionAction());
    this.onYesSelectionActionExecuteAsUserCheckbox = this.addRenderableWidget(
        new Checkbox(this.contentLeftPos + 80, yesSelectionActionTop + 18, "execute_as_player",
            yesSelectionActionData != null && yesSelectionActionData.shouldExecuteAsUser(),
            checkbox -> this.validateYesSelectionAction()));
    this.onYesSelectionActionDebugCheckbox =
        this.addRenderableWidget(new Checkbox(this.contentLeftPos + 215, yesSelectionActionTop + 18,
            "debug", yesSelectionActionData != null && yesSelectionActionData.isDebugEnabled(),
            checkbox -> this.validateYesSelectionAction()));
    this.onYesSelectionActionSaveButton = this.addRenderableWidget(
        new SaveButton(this.onYesSelectionActionBox.x + this.onYesSelectionActionBox.getWidth() + 5,
            yesSelectionActionTop - 2, onPress -> {
              ActionData actionData = new ActionData(ActionType.ON_YES_SELECTION,
                  this.onYesSelectionActionBox.getValue(),
                  this.onYesSelectionActionExecuteAsUserCheckbox.selected(),
                  this.onYesSelectionActionDebugCheckbox.selected());
              NetworkMessageHandler.actionChange(uuid, actionData);
              this.lastYesSelectionActionData = actionData;
              this.onYesSelectionActionSaveButton.active = false;
            }));
    this.onYesSelectionActionSaveButton.active = false;

    // On No Selection Action
    int noSelectionActionTop = this.topPos + 200;
    ActionData noSelectionActionData = this.entity.getActionData(ActionType.ON_NO_SELECTION);
    this.lastNoSelectionActionData = noSelectionActionData;
    this.onNoSelectionActionBox = this.addRenderableWidget(
        actionEditBox(this.contentLeftPos, noSelectionActionTop, noSelectionActionData));
    this.onNoSelectionActionBox.setResponder(consumer -> this.validateNoSelectionAction());
    this.onNoSelectionActionExecuteAsUserCheckbox = this.addRenderableWidget(
        new Checkbox(this.contentLeftPos + 80, noSelectionActionTop + 18, "execute_as_player",
            noSelectionActionData != null && noSelectionActionData.shouldExecuteAsUser(),
            checkbox -> this.validateNoSelectionAction()));
    this.onNoSelectionActionDebugCheckbox =
        this.addRenderableWidget(new Checkbox(this.contentLeftPos + 215, noSelectionActionTop + 18,
            "debug", noSelectionActionData != null && noSelectionActionData.isDebugEnabled(),
            checkbox -> this.validateNoSelectionAction()));
    this.onNoSelectionActionSaveButton = this.addRenderableWidget(
        new SaveButton(this.onNoSelectionActionBox.x + this.onNoSelectionActionBox.getWidth() + 5,
            noSelectionActionTop - 2, onPress -> {
              ActionData actionData =
                  new ActionData(ActionType.ON_NO_SELECTION, this.onNoSelectionActionBox.getValue(),
                      this.onNoSelectionActionExecuteAsUserCheckbox.selected(),
                      this.onNoSelectionActionDebugCheckbox.selected());
              NetworkMessageHandler.actionChange(uuid, actionData);
              this.lastNoSelectionActionData = actionData;
              this.onNoSelectionActionSaveButton.active = false;
            }));
    this.onNoSelectionActionSaveButton.active = false;
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_CONFIGURATION);

    // Description Texts
    this.font.draw(poseStack,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "on_open_dialog"),
        this.contentLeftPos, this.onOpenDialogActionSaveButton.y - 8f, Constants.FONT_COLOR_BLACK);
    this.font.draw(poseStack,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "on_close_dialog"),
        this.contentLeftPos, this.onCloseDialogActionSaveButton.y - 8f, Constants.FONT_COLOR_BLACK);
    this.font.draw(poseStack,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "on_yes_selection"),
        this.contentLeftPos, this.onYesSelectionActionSaveButton.y - 8f,
        Constants.FONT_COLOR_BLACK);
    this.font.draw(poseStack,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "on_no_selection"),
        this.contentLeftPos, this.onNoSelectionActionSaveButton.y - 8f, Constants.FONT_COLOR_BLACK);
  }

}
