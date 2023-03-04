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
import net.minecraft.world.entity.Pose;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.network.NetworkRegistry;
import net.minecraftforge.network.simple.SimpleChannel;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.action.ActionType;
import de.markusbordihn.easynpc.entity.ModelPose;
import de.markusbordihn.easynpc.entity.Profession;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.message.MessageActionChange;
import de.markusbordihn.easynpc.network.message.MessageActionDebug;
import de.markusbordihn.easynpc.network.message.MessageModelPoseChange;
import de.markusbordihn.easynpc.network.message.MessageNameChange;
import de.markusbordihn.easynpc.network.message.MessageOpenConfiguration;
import de.markusbordihn.easynpc.network.message.MessagePoseChange;
import de.markusbordihn.easynpc.network.message.MessageProfessionChange;
import de.markusbordihn.easynpc.network.message.MessageRemoveNPC;
import de.markusbordihn.easynpc.network.message.MessageSaveBasicDialog;
import de.markusbordihn.easynpc.network.message.MessageSaveYesNoDialog;
import de.markusbordihn.easynpc.network.message.MessageScaleChange;
import de.markusbordihn.easynpc.network.message.MessageSkinChange;
import de.markusbordihn.easynpc.network.message.MessageTriggerAction;
import de.markusbordihn.easynpc.network.message.MessageVariantChange;
import de.markusbordihn.easynpc.skin.SkinType;

@EventBusSubscriber
public class NetworkHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String PROTOCOL_VERSION = "5";
  public static final SimpleChannel INSTANCE =
      NetworkRegistry.newSimpleChannel(new ResourceLocation(Constants.MOD_ID, "network"),
          () -> PROTOCOL_VERSION, PROTOCOL_VERSION::equals, PROTOCOL_VERSION::equals);

  private static int id = 0;

  public static void registerNetworkHandler(final FMLCommonSetupEvent event) {

    log.info("{} Network Handler for {} with version {} ...", Constants.LOG_REGISTER_PREFIX,
        INSTANCE, PROTOCOL_VERSION);

    event.enqueueWork(() -> {

      // Action Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageActionChange.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeUtf(message.getActionType());
        buffer.writeUtf(message.getAction());
      }, buffer -> new MessageActionChange(buffer.readUUID(), buffer.readUtf(), buffer.readUtf()),
          MessageActionChange::handle);

      // Action Debug: Client -> Server
      INSTANCE.registerMessage(id++, MessageActionDebug.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeBoolean(message.getDebug());
      }, buffer -> new MessageActionDebug(buffer.readUUID(), buffer.readBoolean()),
          MessageActionDebug::handle);

      // Action Trigger: Client -> Server
      INSTANCE.registerMessage(id++, MessageTriggerAction.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeUtf(message.getActionType());
      }, buffer -> new MessageTriggerAction(buffer.readUUID(), buffer.readUtf()),
          MessageTriggerAction::handle);

      // Name Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageNameChange.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeUtf(message.getName());
      }, buffer -> new MessageNameChange(buffer.readUUID(), buffer.readUtf()),
          MessageNameChange::handle);

      // Open Configuration Screen: Client -> Server
      INSTANCE.registerMessage(id++, MessageOpenConfiguration.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeUtf(message.getDialogName());
      }, buffer -> new MessageOpenConfiguration(buffer.readUUID(), buffer.readUtf()),
          MessageOpenConfiguration::handle);

      // Save Basic Dialog: Client -> Server
      INSTANCE.registerMessage(id++, MessageSaveBasicDialog.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeUtf(message.getDialog());
      }, buffer -> new MessageSaveBasicDialog(buffer.readUUID(), buffer.readUtf()),
          MessageSaveBasicDialog::handle);

      // Save Yes/No Dialog: Client -> Server
      INSTANCE.registerMessage(id++, MessageSaveYesNoDialog.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeUtf(message.getDialog());
        buffer.writeUtf(message.getYesDialog());
        buffer.writeUtf(message.getNoDialog());
        buffer.writeUtf(message.getYesButtonText());
        buffer.writeUtf(message.getNoButtonText());
      }, buffer -> new MessageSaveYesNoDialog(buffer.readUUID(), buffer.readUtf(), buffer.readUtf(),
          buffer.readUtf(), buffer.readUtf(), buffer.readUtf()), MessageSaveYesNoDialog::handle);

      // Model Pose Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageModelPoseChange.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeEnum(message.getModelPose());
      }, buffer -> new MessageModelPoseChange(buffer.readUUID(), buffer.readEnum(ModelPose.class)),
          MessageModelPoseChange::handle);

      // Pose Change: Client -> Server
      INSTANCE.registerMessage(id++, MessagePoseChange.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeEnum(message.getPose());
      }, buffer -> new MessagePoseChange(buffer.readUUID(), buffer.readEnum(Pose.class)),
          MessagePoseChange::handle);

      // Profession Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageProfessionChange.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeEnum(message.getProfession());
      }, buffer -> new MessageProfessionChange(buffer.readUUID(),
          buffer.readEnum(Profession.class)), MessageProfessionChange::handle);

      // Scale Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageScaleChange.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeUtf(message.getScaleAxis());
        buffer.writeFloat(message.getScale());
      }, buffer -> new MessageScaleChange(buffer.readUUID(), buffer.readUtf(), buffer.readFloat()),
          MessageScaleChange::handle);

      // Skin Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageSkinChange.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeUtf(message.getSkin());
        buffer.writeUtf(message.getSkinURL());
        buffer.writeUUID(message.getSkinUUID());
        buffer.writeUtf(message.getSkinType());
      }, buffer -> new MessageSkinChange(buffer.readUUID(), buffer.readUtf(), buffer.readUtf(),
          buffer.readUUID(), buffer.readUtf()), MessageSkinChange::handle);

      // Variant Change: Client -> Server
      INSTANCE.registerMessage(id++, MessageVariantChange.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
        buffer.writeUtf(message.getVariant());
      }, buffer -> new MessageVariantChange(buffer.readUUID(), buffer.readUtf()),
          MessageVariantChange::handle);

      // Remove NPC: Client -> Server
      INSTANCE.registerMessage(id++, MessageRemoveNPC.class, (message, buffer) -> {
        buffer.writeUUID(message.getUUID());
      }, buffer -> new MessageRemoveNPC(buffer.readUUID()), MessageRemoveNPC::handle);
    });
  }

  /** Send action change. */
  public static void actionChange(UUID uuid, ActionType actionType, String action) {
    if (uuid != null && actionType != null && actionType != ActionType.NONE) {
      INSTANCE.sendToServer(new MessageActionChange(uuid, actionType.name(), action));
    }
  }

  /** Send action debug change. */
  public static void actionDebugChange(UUID uuid, boolean debug) {
    if (uuid != null) {
      INSTANCE.sendToServer(new MessageActionDebug(uuid, debug));
    }
  }

  /** Send name change. */
  public static void nameChange(UUID uuid, String name) {
    if (uuid != null && name != null && !name.isEmpty()) {
      INSTANCE.sendToServer(new MessageNameChange(uuid, name));
    }
  }

  /** Open configuration request. */
  public static void openConfiguration(UUID uuid, Enum<ConfigurationType> configurationType) {
    if (uuid != null && configurationType != null) {
      INSTANCE.sendToServer(new MessageOpenConfiguration(uuid, configurationType.name()));
    }
  }

  /** Send model pose change. */
  public static void modelPoseChange(UUID uuid, ModelPose modelPose) {
    if (uuid != null && modelPose != null) {
      INSTANCE.sendToServer(new MessageModelPoseChange(uuid, modelPose));
    }
  }

  /** Send pose change. */
  public static void poseChange(UUID uuid, Pose pose) {
    if (uuid != null && pose != null) {
      INSTANCE.sendToServer(new MessagePoseChange(uuid, pose));
    }
  }

  /** Send profession change. */
  public static void professionChange(UUID uuid, Profession profession) {
    if (uuid != null && profession != null) {
      INSTANCE.sendToServer(new MessageProfessionChange(uuid, profession));
    }
  }

  /** Send remove NPC. */
  public static void removeNPC(UUID uuid) {
    if (uuid != null) {
      INSTANCE.sendToServer(new MessageRemoveNPC(uuid));
    }
  }

  /** Save basic dialog. */
  public static void saveBasicDialog(UUID uuid, String dialog) {
    if (uuid != null && dialog != null) {
      INSTANCE.sendToServer(new MessageSaveBasicDialog(uuid, dialog));
    }
  }

  /** Save yes/no dialog. */
  public static void saveYesNoDialog(UUID uuid, String dialog, String yesDialog, String noDialog,
      String yesButtonText, String noButtonText) {
    if (uuid != null && dialog != null && yesDialog != null && noDialog != null) {
      INSTANCE.sendToServer(new MessageSaveYesNoDialog(uuid, dialog, yesDialog, noDialog,
          yesButtonText, noButtonText));
    }
  }

  /** Send scale change. */
  public static void scaleChange(UUID uuid, String scaleAxis, float scale) {
    if (uuid != null && scaleAxis != null) {
      INSTANCE.sendToServer(new MessageScaleChange(uuid, scaleAxis, scale));
    }
  }

  /** Send skin change. */
  public static void skinChange(UUID uuid, Enum<SkinType> skinType) {
    if (uuid != null && skinType != null) {
      INSTANCE
          .sendToServer(new MessageSkinChange(uuid, "", "", Constants.BLANK_UUID, skinType.name()));
    }
  }

  public static void skinChange(UUID uuid, String skin, Enum<SkinType> skinType) {
    if (uuid != null && skin != null && skinType != null) {
      INSTANCE.sendToServer(
          new MessageSkinChange(uuid, skin, "", Constants.BLANK_UUID, skinType.name()));
    }
  }

  public static void skinChange(UUID uuid, String skin, String skinURL, UUID skinUUID,
      Enum<SkinType> skinType) {
    if (uuid != null && skin != null && skinType != null) {
      INSTANCE.sendToServer(new MessageSkinChange(uuid, skin, skinURL, skinUUID, skinType.name()));
    }
  }

  /** Send trigger action. */
  public static void triggerAction(UUID uuid, ActionType actionType) {
    if (uuid != null && actionType != null && actionType != ActionType.NONE) {
      INSTANCE.sendToServer(new MessageTriggerAction(uuid, actionType.name()));
    }
  }

  /** Send variant change. */
  public static void variantChange(UUID uuid, Enum<?> variant) {
    if (uuid != null && variant != null) {
      INSTANCE.sendToServer(new MessageVariantChange(uuid, variant.name()));
    }
  }

}
