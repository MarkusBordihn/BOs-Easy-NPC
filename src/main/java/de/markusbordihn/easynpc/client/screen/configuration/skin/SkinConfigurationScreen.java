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

package de.markusbordihn.easynpc.client.screen.configuration.skin;

import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.NetworkMessage;

@OnlyIn(Dist.CLIENT)
public class SkinConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  // NPC Entity
  protected final boolean isPlayerSkinModel;

  // Buttons
  protected Button customSkinButton = null;
  protected Button defaultSkinButton = null;
  protected Button playerSkinButton = null;

  // Settings
  protected int skinPreviewWidth = 56;

  public SkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.isPlayerSkinModel =
        SkinModel.HUMANOID.equals(this.skinModel) || SkinModel.HUMANOID_SLIM.equals(this.skinModel);
  }

  @Override
  public void init() {
    super.init();

    // Skin Types
    int skinButtonWidth = 92;
    this.defaultSkinButton = this.addRenderableWidget(menuButton(this.buttonLeftPos,
        this.buttonTopPos, skinButtonWidth - 4, "default_skin", onPress -> {
          NetworkMessage.openConfiguration(uuid, ConfigurationType.DEFAULT_SKIN);
        }));
    this.playerSkinButton =
        this.addRenderableWidget(menuButton(this.buttonLeftPos + this.defaultSkinButton.getWidth(),
            this.buttonTopPos, skinButtonWidth, "player_skin", onPress -> {
              NetworkMessage.openConfiguration(uuid, ConfigurationType.PLAYER_SKIN);
            }));
    this.customSkinButton = this.addRenderableWidget(menuButton(
        this.buttonLeftPos + this.defaultSkinButton.getWidth() + this.playerSkinButton.getWidth(),
        this.buttonTopPos, skinButtonWidth, "custom_skin", onPress -> {
          NetworkMessage.openConfiguration(uuid, ConfigurationType.CUSTOM_SKIN);
        }));

    // Default button stats
    this.customSkinButton.active = this.hasPermissions(COMMON.customSkinConfigurationEnabled.get(),
        COMMON.customSkinConfigurationAllowInCreative.get(),
        COMMON.customSkinConfigurationPermissionLevel.get());
    this.defaultSkinButton.active =
        this.hasPermissions(COMMON.defaultSkinConfigurationEnabled.get(),
            COMMON.defaultSkinConfigurationAllowInCreative.get(),
            COMMON.defaultSkinConfigurationPermissionLevel.get());
    this.playerSkinButton.active = this.hasPermissions(COMMON.playerSkinConfigurationEnabled.get(),
        COMMON.playerSkinConfigurationAllowInCreative.get(),
        COMMON.playerSkinConfigurationPermissionLevel.get());
  }

}
