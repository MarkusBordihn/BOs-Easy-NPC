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

package de.markusbordihn.easynpc.configui.client.screen.configuration.skin;

import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.client.texture.TextureManager;
import de.markusbordihn.easynpc.client.texture.TextureModelKey;
import de.markusbordihn.easynpc.configui.client.screen.components.ReloadButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.utils.PlayersUtils;
import de.markusbordihn.easynpc.validator.NameValidator;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class PlayerSkinConfigurationScreen<T extends ConfigurationMenu>
    extends TextureLocationSkinConfigurationScreen<T> {

  private static final int RELOAD_BUTTON_HEIGHT = 16;
  private static final int RELOAD_BUTTON_WIDTH = 20;

  public PlayerSkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component, "use_a_player_name", 85, 71);
  }

  @Override
  protected Set<UUID> getTextureCacheKeys(SkinModel skinModel) {
    return PlayerTextureManager.getTextureCacheKeys(skinModel);
  }

  @Override
  protected SkinType getTextureSkinType(TextureModelKey textureModelKey) {
    return PlayerTextureManager.getTextureSkinType(textureModelKey);
  }

  @Override
  protected SkinDataEntry createSkinDataEntry(TextureModelKey textureModelKey) {
    return SkinDataEntry.createPlayerSkin("", textureModelKey.getUUID());
  }

  @Override
  protected boolean isValidTextureSkinLocation(String textureSkinLocation) {
    return NameValidator.isValidPlayerName(textureSkinLocation);
  }

  @Override
  protected void defineAdditionalSkinButtons(Button skinButton, TextureModelKey textureModelKey) {
    this.skinButtons.add(
        new ReloadButton(
            skinButton.getX() + skinButton.getWidth() - RELOAD_BUTTON_WIDTH,
            skinButton.getY() + skinButton.getHeight() - RELOAD_BUTTON_HEIGHT,
            RELOAD_BUTTON_WIDTH,
            RELOAD_BUTTON_HEIGHT,
            null,
            onPress -> PlayerTextureManager.refreshTexture(textureModelKey)));
  }

  @Override
  protected void addTextureSkinLocation() {
    String textureSkinLocationValue = this.textureSkinLocationBox.getValue();
    if (!textureSkinLocationValue.isEmpty()
        && !textureSkinLocationValue.equals(this.formerTextureSkinLocation)) {
      if (!NameValidator.isValidPlayerName(textureSkinLocationValue)) {
        this.errorMessage = "invalid_player_name";
        return;
      }

      this.addTextureSettingsButton.active = false;
      this.formerTextureSkinLocation = textureSkinLocationValue;
      updateNextTextureSkinLocationChange();
      CompletableFuture.supplyAsync(() -> PlayersUtils.getUserUUID(textureSkinLocationValue))
          .thenAcceptAsync(
              playerUUID -> {
                if (playerUUID == null) {
                  this.errorMessage = "invalid_player_uuid";
                  return;
                }

                log.debug(
                    "Setting player texture to {} with UUID {}",
                    textureSkinLocationValue,
                    playerUUID);
                TextureManager.clearLastErrorMessage();
                this.errorMessage = "";
                NetworkMessageHandlerManager.getServerHandler()
                    .setSkin(
                        this.getEasyNPCUUID(),
                        SkinDataEntry.createPlayerSkin(textureSkinLocationValue, playerUUID));
              },
              Minecraft.getInstance());
    }
  }

  @Override
  public void init() {
    super.init();

    this.playerSkinButton.active = false;

    this.setDescriptionText("player_skin.text");
  }
}
