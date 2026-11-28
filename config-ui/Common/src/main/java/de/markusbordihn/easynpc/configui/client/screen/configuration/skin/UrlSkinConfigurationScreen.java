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

import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.client.texture.TextureManager;
import de.markusbordihn.easynpc.client.texture.TextureModelKey;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.configui.validator.RemoteImageValidator;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.validator.UrlValidator;
import java.net.URL;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class UrlSkinConfigurationScreen<T extends ConfigurationMenu>
    extends TextureLocationSkinConfigurationScreen<T> {

  public UrlSkinConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component, "use_a_skin_url", 193, 73);
  }

  @Override
  protected Set<UUID> getTextureCacheKeys(SkinModel skinModel) {
    return RemoteTextureManager.getTextureCacheKeys(skinModel);
  }

  @Override
  protected SkinType getTextureSkinType(TextureModelKey textureModelKey) {
    return RemoteTextureManager.getTextureSkinType(textureModelKey);
  }

  @Override
  protected SkinDataEntry createSkinDataEntry(TextureModelKey textureModelKey) {
    return SkinDataEntry.createRemoteSkin(RemoteTextureManager.getTextureSkinURL(textureModelKey));
  }

  @Override
  protected boolean isValidTextureSkinLocation(String textureSkinLocation) {
    return UrlValidator.isValidUrl(textureSkinLocation);
  }

  @Override
  protected void addTextureSkinLocation() {
    String textureSkinLocationValue = this.textureSkinLocationBox.getValue();
    if (textureSkinLocationValue.isEmpty()
        || textureSkinLocationValue.equals(this.formerTextureSkinLocation)) {
      return;
    }

    updateNextTextureSkinLocationChange();
    this.addTextureSettingsButton.active = false;
    this.formerTextureSkinLocation = textureSkinLocationValue;

    if (!UrlValidator.isValidUrl(textureSkinLocationValue)) {
      this.errorMessage = "invalid_remote_image";
      this.resetCooldownOnError();
      return;
    }

    final URL textureSkinLocation;
    try {
      textureSkinLocation = new URL(textureSkinLocationValue);
    } catch (Exception e) {
      log.error("Invalid URL format: {}", textureSkinLocationValue, e);
      this.errorMessage = "invalid_url";
      this.resetCooldownOnError();
      return;
    }

    // RemoteImageValidator.isValidImage blocks on network I/O; keep it off the render thread.
    CompletableFuture.supplyAsync(() -> RemoteImageValidator.isValidImage(textureSkinLocation))
        .whenComplete((valid, throwable) ->
            Minecraft.getInstance()
                .execute(
                    () -> this.onRemoteImageValidated(textureSkinLocationValue, valid, throwable)));
  }

  private void onRemoteImageValidated(
      String textureSkinLocationValue, Boolean valid, Throwable throwable) {
    if (this.minecraft == null || this.minecraft.screen != this) {
      return;
    }

    if (throwable != null) {
      log.error(
          "Error validating remote image from {}: {}",
          textureSkinLocationValue,
          throwable.getMessage());
      this.errorMessage = "error_loading_image";
      this.resetCooldownOnError();
      return;
    }

    if (!Boolean.TRUE.equals(valid)) {
      log.warn("Unable to set remote user texture to {}", textureSkinLocationValue);
      this.errorMessage = "invalid_remote_image";
      this.resetCooldownOnError();
      return;
    }

    log.debug("Setting remote user texture to {}", textureSkinLocationValue);
    TextureManager.clearLastErrorMessage();
    this.errorMessage = "";
    NetworkMessageHandlerManager.getServerHandler()
        .setSkin(this.getEasyNPCUUID(), SkinDataEntry.createRemoteSkin(textureSkinLocationValue));
  }

  private void resetCooldownOnError() {
    this.formerTextureSkinLocation = "";
    SkinConfigurationScreen.nextTextureSkinLocationChange = 0;
  }

  @Override
  public void init() {
    super.init();

    this.urlSkinButton.active = false;

    this.setDescriptionText("url_skin.text");
  }
}
