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
import de.markusbordihn.easynpc.client.screen.components.AddButton;
import de.markusbordihn.easynpc.client.screen.components.EditButton;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.message.NetworkMessageHandlerManager;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class ActionConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  protected Button basicActionButton = null;
  protected Button dialogActionButton = null;
  protected Button distanceActionButton = null;

  public ActionConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  public EditBox actionEditBox(int left, int top, ActionDataEntry actionDataEntry) {
    EditBox editBox = new TextField(this.font, left, top, 275, 16);
    editBox.setMaxLength(255);
    editBox.setValue(actionDataEntry != null ? actionDataEntry.getCommand() : "");
    return editBox;
  }

  @Override
  public void init() {
    super.init();

    // Action Types
    this.basicActionButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                80,
                "basic",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.BASIC_ACTION)));
    this.dialogActionButton =
        this.addRenderableWidget(
            new TextButton(
                this.basicActionButton.getX() + this.basicActionButton.getWidth(),
                this.buttonTopPos,
                80,
                "dialog_actions",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.DIALOG_ACTION)));
    this.distanceActionButton =
        this.addRenderableWidget(
            new TextButton(
                this.dialogActionButton.getX() + this.dialogActionButton.getWidth(),
                this.buttonTopPos,
                80,
                "distance_actions",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.DISTANCE_ACTION)));
  }

  protected Button getActionDataButton(
      int left, int top, ActionEventType actionEventType, ConfigurationType configurationType) {
    int buttonWidth = 300;
    ActionDataSet actionDataSet = this.getActionEventSet().getActionEvents(actionEventType);
    if (actionDataSet == null || actionDataSet.isEmpty()) {
      Component buttonLabel =
          Component.translatable(
              Constants.TEXT_CONFIG_PREFIX + "add_action",
              Component.translatable(
                  Constants.TEXT_CONFIG_PREFIX + actionEventType.name().toLowerCase()));
      return new AddButton(
              left,
              top,
              buttonWidth,
              buttonLabel,
              onPress ->
                  NetworkMessageHandlerManager.getServerHandler()
                      .openActionDataEntryEditor(
                          this.getNpcUUID(),
                          actionEventType,
                          configurationType,
                          ActionDataEntry.EMPTY))
          .setRenderCenter(false);
    } else {
      Component buttonLabel =
          Component.translatable(
              Constants.TEXT_CONFIG_PREFIX + "edit_action",
              Component.translatable(
                  Constants.TEXT_CONFIG_PREFIX + actionEventType.name().toLowerCase()));
      return new EditButton(
              left,
              top,
              buttonWidth,
              buttonLabel,
              onPress ->
                  NetworkMessageHandlerManager.getServerHandler()
                      .openActionDataEditor(this.getNpcUUID(), actionEventType, configurationType))
          .setRenderCenter(false);
    }
  }
}
