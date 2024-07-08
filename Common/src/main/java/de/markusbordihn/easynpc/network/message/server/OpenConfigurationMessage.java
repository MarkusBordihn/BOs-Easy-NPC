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

package de.markusbordihn.easynpc.network.message.server;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class OpenConfigurationMessage extends NetworkMessage<OpenConfigurationMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "open_configuration_screen");

  private final ConfigurationType configurationType;

  public OpenConfigurationMessage(
      final UUID uuid, final ConfigurationType configurationType, final int pageIndex) {
    super(uuid, pageIndex);
    this.configurationType = configurationType;
  }

  public static OpenConfigurationMessage decode(final FriendlyByteBuf buffer) {
    return new OpenConfigurationMessage(
        buffer.readUUID(), buffer.readEnum(ConfigurationType.class), buffer.readInt());
  }

  public static FriendlyByteBuf encode(
      final OpenConfigurationMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getConfigurationType());
    buffer.writeInt(message.pageIndex);
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(
      final OpenConfigurationMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate dialog name.
    ConfigurationType configurationType = message.getConfigurationType();
    if (configurationType == null) {
      log.error("Invalid configuration type for {} from {}", message, serverPlayer);
      return;
    }

    // Validate page index.
    int pageIndex = message.getPageIndex();
    if (pageIndex < 0) {
      log.error("Invalid page index {} for {} from {}", pageIndex, message, serverPlayer);
      return;
    }

    // Open configuration screen
    MenuManager.getMenuHandler()
        .openConfigurationMenu(configurationType, serverPlayer, message.getEasyNPC(), pageIndex);
  }

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public OpenConfigurationMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public ConfigurationType getConfigurationType() {
    return this.configurationType;
  }
}
