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

package de.markusbordihn.easynpc.client.screen.configuration.attribute;

import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.configuration.attribute.AttributeConfigurationMenu;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class AttributeConfigurationScreen<T extends AttributeConfigurationMenu>
    extends ConfigurationScreen<T> {

  protected Button abilitiesAttributeButton = null;
  protected Button baseAttributeButton = null;
  protected Button displayAttributeButton = null;

  public AttributeConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Objective Types
    this.abilitiesAttributeButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                76,
                "abilities",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.ABILITIES_ATTRIBUTE)));

    this.baseAttributeButton =
        this.addRenderableWidget(
            new TextButton(
                this.abilitiesAttributeButton.getX() + this.abilitiesAttributeButton.getWidth(),
                this.buttonTopPos,
                60,
                "base",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.BASE_ATTRIBUTE)));

    this.displayAttributeButton =
        this.addRenderableWidget(
            new TextButton(
                this.baseAttributeButton.getX() + this.baseAttributeButton.getWidth(),
                this.buttonTopPos,
                70,
                "display",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.DISPLAY_ATTRIBUTE)));

    // Default button stats
    this.abilitiesAttributeButton.active =
        this.hasPermissions(
            COMMON.abilitiesAttributeConfigurationEnabled.get(),
            COMMON.abilitiesAttributeConfigurationAllowInCreative.get(),
            COMMON.abilitiesAttributeConfigurationPermissionLevel.get());
    this.baseAttributeButton.active =
        this.hasPermissions(
            COMMON.baseAttributeConfigurationEnabled.get(),
            COMMON.baseAttributeConfigurationAllowInCreative.get(),
            COMMON.baseAttributeConfigurationPermissionLevel.get());
    this.displayAttributeButton.active =
        this.hasPermissions(
            COMMON.displayAttributeConfigurationEnabled.get(),
            COMMON.displayAttributeConfigurationAllowInCreative.get(),
            COMMON.displayAttributeConfigurationPermissionLevel.get());
  }
}
