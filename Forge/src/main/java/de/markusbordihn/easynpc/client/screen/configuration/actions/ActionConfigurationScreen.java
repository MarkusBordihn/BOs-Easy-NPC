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

import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.configuration.action.ActionConfigurationMenu;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class ActionConfigurationScreen<T extends ActionConfigurationMenu>
    extends ConfigurationScreen<T> {

  protected final ActionEventSet actionDataSet;
  protected Button basicActionButton = null;
  protected Button dialogActionButton = null;
  protected Button distanceActionButton = null;

  public ActionConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.actionDataSet = menu.getActionEventSet();
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
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.BASIC_ACTION)));
    this.dialogActionButton =
        this.addRenderableWidget(
            new TextButton(
                this.basicActionButton.x + this.basicActionButton.getWidth(),
                this.buttonTopPos,
                80,
                "dialog_actions",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.DIALOG_ACTION)));
    this.distanceActionButton =
        this.addRenderableWidget(
            new TextButton(
                this.dialogActionButton.x + this.dialogActionButton.getWidth(),
                this.buttonTopPos,
                80,
                "distance_actions",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.DISTANCE_ACTION)));

    // Default button stats
    this.basicActionButton.active =
        this.hasPermissions(
            COMMON.basicActionConfigurationEnabled.get(),
            COMMON.basicActionConfigurationAllowInCreative.get(),
            COMMON.basicActionConfigurationPermissionLevel.get());
    this.dialogActionButton.active =
        this.hasPermissions(
            COMMON.dialogActionConfigurationEnabled.get(),
            COMMON.dialogActionConfigurationAllowInCreative.get(),
            COMMON.dialogActionConfigurationPermissionLevel.get());
    this.distanceActionButton.active =
        this.hasPermissions(
            COMMON.distanceActionConfigurationEnabled.get(),
            COMMON.distanceActionConfigurationAllowInCreative.get(),
            COMMON.distanceActionConfigurationPermissionLevel.get());
  }
}
