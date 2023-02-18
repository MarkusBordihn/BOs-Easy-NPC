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

package de.markusbordihn.easynpc.network.message;

import java.util.UUID;
import java.util.function.Supplier;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.level.ServerPlayer;

import net.minecraftforge.network.NetworkEvent;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.action.ActionType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;

public class MessageTriggerAction {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final String actionType;
  protected final UUID uuid;

  public MessageTriggerAction(UUID uuid, String actionType) {
    this.uuid = uuid;
    this.actionType = actionType;
  }

  public String getActionType() {
    return this.actionType;
  }

  public UUID getUUID() {
    return this.uuid;
  }

  public static void handle(MessageTriggerAction message,
      Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageTriggerAction message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || uuid == null) {
      log.error("Unable to trigger action with message {} from {}", message, context);
      return;
    }

    // Validate action type.
    ActionType actionType = ActionType.get(message.getActionType());
    if (actionType == null || actionType == ActionType.NONE) {
      log.error("Invalid action type {} for {} from {}", actionType, message, serverPlayer);
      return;
    }

    // Validate entity.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPCEntity == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return;
    }

    // Validate action.
    if (!easyNPCEntity.hasAction(actionType)) {
      log.error("Unknown trigger action {} request for UUID {} from {}", actionType, uuid,
          serverPlayer);
      return;
    }

    // Perform action.
    String action = easyNPCEntity.getAction(actionType);
    int permissionLevel = easyNPCEntity.getActionPermissionLevel();
    log.debug("Trigger action {}:{} for {} from {} with permission level {} ...", actionType,
        action, easyNPCEntity, serverPlayer, permissionLevel);
    easyNPCEntity.executeAction(actionType);
  }

}
