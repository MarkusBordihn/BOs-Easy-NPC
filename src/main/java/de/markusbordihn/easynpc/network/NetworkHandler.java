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

package de.markusbordihn.easynpc.network;

import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.resources.ResourceLocation;

import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.network.NetworkRegistry;
import net.minecraftforge.network.simple.SimpleChannel;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.dialog.DialogType;
import de.markusbordihn.easynpc.network.message.MessageNameChange;
import de.markusbordihn.easynpc.network.message.MessageOpenDialog;
import de.markusbordihn.easynpc.network.message.MessageProfessionChange;
import de.markusbordihn.easynpc.network.message.MessageSaveDialog;
import de.markusbordihn.easynpc.network.message.MessageVariantChange;

@EventBusSubscriber
public class NetworkHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String PROTOCOL_VERSION = "1";
  public static final SimpleChannel INSTANCE =
      NetworkRegistry.newSimpleChannel(new ResourceLocation(Constants.MOD_ID, "network"),
          () -> PROTOCOL_VERSION, PROTOCOL_VERSION::equals, PROTOCOL_VERSION::equals);

  private static int id = 0;

  public static void registerNetworkHandler(final FMLCommonSetupEvent event) {

    log.info("{} Network Handler for {} with version {} ...", Constants.LOG_REGISTER_PREFIX,
        INSTANCE, PROTOCOL_VERSION);

    event.enqueueWork(() -> {

      // Name Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageNameChange.class, (message, buffer) -> {
        buffer.writeUtf(message.getUUID());
        buffer.writeUtf(message.getName());
      }, buffer -> new MessageNameChange(buffer.readUtf(), buffer.readUtf()),
          MessageNameChange::handle);

      // Open Dialog Request: Client -> Server
      INSTANCE.registerMessage(id++, MessageOpenDialog.class, (message, buffer) -> {
        buffer.writeUtf(message.getUUID());
        buffer.writeUtf(message.getDialogName());
      }, buffer -> new MessageOpenDialog(buffer.readUtf(), buffer.readUtf()),
          MessageOpenDialog::handle);

      // Save Dialog Request: Client -> Server
      INSTANCE.registerMessage(id++, MessageSaveDialog.class, (message, buffer) -> {
        buffer.writeUtf(message.getUUID());
        buffer.writeUtf(message.getDialogType().name());
        buffer.writeUtf(message.getDialog());
      }, buffer -> new MessageSaveDialog(buffer.readUtf(), buffer.readUtf(), buffer.readUtf()),
          MessageSaveDialog::handle);

      // Profession Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageProfessionChange.class, (message, buffer) -> {
        buffer.writeUtf(message.getUUID());
        buffer.writeUtf(message.getProfession());
      }, buffer -> new MessageProfessionChange(buffer.readUtf(), buffer.readUtf()),
          MessageProfessionChange::handle);

      // Variant Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageVariantChange.class, (message, buffer) -> {
        buffer.writeUtf(message.getUUID());
        buffer.writeUtf(message.getVariant());
      }, buffer -> new MessageVariantChange(buffer.readUtf(), buffer.readUtf()),
          MessageVariantChange::handle);
    });
  }

  /** Send name change. */
  public static void nameChange(UUID uuid, String name) {
    if (uuid != null && name != null && !name.isEmpty()) {
      INSTANCE.sendToServer(new MessageNameChange(uuid.toString(), name));
    }
  }

  /** Open dialog request. */
  public static void openDialog(UUID uuid, String dialogName) {
    if (uuid != null && dialogName != null && !dialogName.isEmpty()) {
      INSTANCE.sendToServer(new MessageOpenDialog(uuid.toString(), dialogName));
    }
  }

  /** Save dialog. */
  public static void saveDialog(UUID uuid, DialogType dialogType, String dialog) {
    if (uuid != null && dialogType != null && dialog != null) {
      INSTANCE.sendToServer(new MessageSaveDialog(uuid.toString(), dialogType.name(), dialog));
    }
  }

  /** Send profession change. */
  public static void professionChange(UUID uuid, Enum<?> profession) {
    if (uuid != null && profession != null) {
      INSTANCE.sendToServer(new MessageProfessionChange(uuid.toString(), profession.name()));
    }
  }

  /** Send variant change. */
  public static void variantChange(UUID uuid, Enum<?> variant) {
    if (uuid != null && variant != null) {
      INSTANCE.sendToServer(new MessageVariantChange(uuid.toString(), variant.name()));
    }
  }
}
