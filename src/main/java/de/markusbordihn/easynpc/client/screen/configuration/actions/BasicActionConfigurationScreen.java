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

import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.Checkbox;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.action.ActionType;
import de.markusbordihn.easynpc.menu.configuration.action.BasicActionConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkHandler;

@OnlyIn(Dist.CLIENT)
public class BasicActionConfigurationScreen
    extends ActionConfigurationScreen<BasicActionConfigurationMenu> {

  protected EditBox onInteractionActionBox;
  protected EditBox onOpenActionBox;
  protected EditBox onCloseActionBox;
  protected EditBox onYesActionBox;
  protected EditBox onNoActionBox;
  protected Button saveOnInteractionActionButton;
  protected Button saveOnOpenActionButton;
  protected Button saveOnCloseActionButton;
  protected Button saveOnYesActionButton;
  protected Button saveOnNoActionButton;
  protected Checkbox debugActionCheckbox;

  // Cache
  private String lastInteractionAction = "";
  private String lastOpenAction = "";
  private String lastCloseAction = "";
  private String lastYesAction = "";
  private String lastNoAction = "";

  public BasicActionConfigurationScreen(BasicActionConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  public void validateInteractionAction() {
    String action = this.onInteractionActionBox.getValue();
    this.saveOnInteractionActionButton.active =
        action == null || (action.isEmpty() && !this.lastInteractionAction.isEmpty())
            || !action.equals(this.lastInteractionAction);
  }

  public void validateOpenAction() {
    String action = this.onOpenActionBox.getValue();
    this.saveOnOpenActionButton.active =
        action == null || (action.isEmpty() && !this.lastOpenAction.isEmpty())
            || !action.equals(this.lastOpenAction);
  }

  public void validateCloseAction() {
    String action = this.onCloseActionBox.getValue();
    this.saveOnCloseActionButton.active =
        action == null || (action.isEmpty() && !this.lastOpenAction.isEmpty())
            || !action.equals(this.lastCloseAction);
  }

  public void validateYesAction() {
    String action = this.onYesActionBox.getValue();
    this.saveOnYesActionButton.active =
        action == null || (action.isEmpty() && !this.lastOpenAction.isEmpty())
            || !action.equals(this.lastYesAction);
  }

  public void validateNoAction() {
    String action = this.onNoActionBox.getValue();
    this.saveOnNoActionButton.active =
        action == null || (action.isEmpty() && !this.lastOpenAction.isEmpty())
            || !action.equals(this.lastNoAction);
  }

  @Override
  public void init() {
    super.init();

    // On Interaction Action
    int interactionActionTop = this.topPos + 50;
    this.lastInteractionAction = this.entity.getAction(ActionType.ON_INTERACTION);
    this.onInteractionActionBox = new EditBox(this.font, this.contentLeftPos, interactionActionTop,
        256, 20, new TextComponent("On Interaction Action"));
    this.onInteractionActionBox.setMaxLength(255);
    this.onInteractionActionBox.setValue(this.lastInteractionAction);
    this.onInteractionActionBox.setResponder(consumer -> this.validateInteractionAction());
    this.addRenderableWidget(this.onInteractionActionBox);
    this.saveOnInteractionActionButton = this.addRenderableWidget(new Button(this.leftPos + 265,
        interactionActionTop, 25, 20, new TextComponent(""), onPress -> {
          String action = this.onInteractionActionBox.getValue();
          NetworkHandler.actionChange(uuid, ActionType.ON_INTERACTION, action);
          this.lastInteractionAction = action;
          this.saveOnInteractionActionButton.active = false;
        }));
    this.saveOnInteractionActionButton.active = false;

    // On Open Dialog Action
    int openActionTop = this.topPos + 84;
    this.lastOpenAction = this.entity.getAction(ActionType.ON_OPEN_DIALOG);
    this.onOpenActionBox = new EditBox(this.font, this.contentLeftPos, openActionTop, 256, 20,
        new TextComponent("On Open Action"));
    this.onOpenActionBox.setMaxLength(255);
    this.onOpenActionBox.setValue(this.lastOpenAction);
    this.onOpenActionBox.setResponder(consumer -> this.validateOpenAction());
    this.addRenderableWidget(this.onOpenActionBox);
    this.saveOnOpenActionButton = this.addRenderableWidget(
        new Button(this.leftPos + 265, openActionTop, 25, 20, new TextComponent(""), onPress -> {
          String action = this.onOpenActionBox.getValue();
          NetworkHandler.actionChange(uuid, ActionType.ON_OPEN_DIALOG, action);
          this.lastOpenAction = action;
          this.saveOnOpenActionButton.active = false;
        }));
    this.saveOnOpenActionButton.active = false;

    // On Close Dialog Action
    int closeActionTop = this.topPos + 118;
    this.lastCloseAction = this.entity.getAction(ActionType.ON_CLOSE_DIALOG);
    this.onCloseActionBox = new EditBox(this.font, this.contentLeftPos, closeActionTop, 256, 20,
        new TextComponent("On Close Action"));
    this.onCloseActionBox.setMaxLength(255);
    this.onCloseActionBox.setValue(this.lastCloseAction);
    this.onCloseActionBox.setResponder(consumer -> this.validateCloseAction());
    this.addRenderableWidget(this.onCloseActionBox);
    this.saveOnCloseActionButton = this.addRenderableWidget(
        new Button(this.leftPos + 265, closeActionTop, 25, 20, new TextComponent(""), onPress -> {
          String action = this.onCloseActionBox.getValue();
          NetworkHandler.actionChange(uuid, ActionType.ON_CLOSE_DIALOG, action);
          this.lastCloseAction = action;
          this.saveOnCloseActionButton.active = false;
        }));
    this.saveOnCloseActionButton.active = false;

    // On Yes Selection Action
    int yesActionTop = this.topPos + 152;
    this.lastYesAction = this.entity.getAction(ActionType.ON_YES_SELECTION);
    this.onYesActionBox = new EditBox(this.font, this.contentLeftPos, yesActionTop, 256, 20,
        new TextComponent("On Yes Action"));
    this.onYesActionBox.setMaxLength(255);
    this.onYesActionBox.setValue(this.lastYesAction);
    this.onYesActionBox.setResponder(consumer -> this.validateYesAction());
    this.addRenderableWidget(this.onYesActionBox);
    this.saveOnYesActionButton = this.addRenderableWidget(
        new Button(this.leftPos + 265, yesActionTop, 25, 20, new TextComponent(""), onPress -> {
          String action = this.onYesActionBox.getValue();
          NetworkHandler.actionChange(uuid, ActionType.ON_YES_SELECTION, action);
          this.lastYesAction = action;
          this.saveOnYesActionButton.active = false;
        }));
    this.saveOnYesActionButton.active = false;

    // On No Selection Action
    int noActionTop = this.topPos + 186;
    this.lastNoAction = this.entity.getAction(ActionType.ON_NO_SELECTION);
    this.onNoActionBox = new EditBox(this.font, this.contentLeftPos, noActionTop, 256, 20,
        new TextComponent("On No Action"));
    this.onNoActionBox.setMaxLength(255);
    this.onNoActionBox.setValue(this.lastNoAction);
    this.onNoActionBox.setResponder(consumer -> this.validateNoAction());
    this.addRenderableWidget(this.onNoActionBox);
    this.saveOnNoActionButton = this.addRenderableWidget(
        new Button(this.leftPos + 265, noActionTop, 25, 20, new TextComponent(""), onPress -> {
          String action = this.onNoActionBox.getValue();
          NetworkHandler.actionChange(uuid, ActionType.ON_NO_SELECTION, action);
          this.lastNoAction = action;
          this.saveOnNoActionButton.active = false;
        }));
    this.saveOnNoActionButton.active = false;

    // Debug Action Option
    this.debugActionCheckbox = this.addRenderableWidget(new Checkbox(this.contentLeftPos,
        this.topPos + 211, 20, 20, new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + "debug")
            .withStyle(ChatFormatting.WHITE),
        this.entity.getActionDebug()) {
      @Override
      public void onPress() {
        NetworkHandler.actionDebugChange(uuid, !entity.getActionDebug());
        super.onPress();
      }
    });

    // Default button stats
    this.basicActionButton.active = true;
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_CONFIGURATION);

    // Button Icons
    this.blit(poseStack, this.saveOnInteractionActionButton.x + 5,
        this.saveOnInteractionActionButton.y + 3, 60,
        this.saveOnInteractionActionButton.active ? 0 : 16, 16, 16);
    this.blit(poseStack, this.saveOnOpenActionButton.x + 5, this.saveOnOpenActionButton.y + 3, 60,
        this.saveOnOpenActionButton.active ? 0 : 16, 16, 16);
    this.blit(poseStack, this.saveOnCloseActionButton.x + 5, this.saveOnCloseActionButton.y + 3, 60,
        this.saveOnCloseActionButton.active ? 0 : 16, 16, 16);
    this.blit(poseStack, this.saveOnYesActionButton.x + 5, this.saveOnYesActionButton.y + 3, 60,
        this.saveOnYesActionButton.active ? 0 : 16, 16, 16);
    this.blit(poseStack, this.saveOnNoActionButton.x + 5, this.saveOnNoActionButton.y + 3, 60,
        this.saveOnNoActionButton.active ? 0 : 16, 16, 16);

    // Description Texts
    this.font.draw(poseStack,
        new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + "on_interaction"),
        this.contentLeftPos, this.saveOnInteractionActionButton.y - 9f, Constants.FONT_COLOR_BLACK);
    this.font.draw(poseStack,
        new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + "on_open_dialog"),
        this.contentLeftPos, this.saveOnOpenActionButton.y - 9f, Constants.FONT_COLOR_BLACK);
    this.font.draw(poseStack,
        new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + "on_close_dialog"),
        this.contentLeftPos, this.saveOnCloseActionButton.y - 9f, Constants.FONT_COLOR_BLACK);
    this.font.draw(poseStack,
        new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + "on_yes_selection"),
        this.contentLeftPos, this.saveOnYesActionButton.y - 9f, Constants.FONT_COLOR_BLACK);
    this.font.draw(poseStack,
        new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + "on_no_selection"),
        this.contentLeftPos, this.saveOnNoActionButton.y - 9f, Constants.FONT_COLOR_BLACK);
  }

}
