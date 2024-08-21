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

package de.markusbordihn.easynpc.access;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class AccessManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private AccessManager() {}

  public static boolean hasAccess(CommandSourceStack context, UUID uuid) {
    if (context == null || uuid == null) {
      return false;
    }

    // Check if server player is available and skip access check if not.
    try {
      ServerPlayer serverPlayer = context.getPlayerOrException();
      boolean hasAccess = hasAccess(serverPlayer, uuid);
      if (hasAccess) {
        log.debug(
            "[Access allowed] Player {} has access to EasyNPC with UUID {}!", serverPlayer, uuid);
      } else {
        log.error(
            "[Access denied] Player {} tried to access EasyNPC with UUID {}!", serverPlayer, uuid);
      }
      return hasAccess;
    } catch (CommandSyntaxException serverPlayerException) {
      try {
        // Allow access from the same entity.
        Entity entity = context.getEntityOrException();
        EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, context.getLevel());
        if (easyNPC != null && easyNPC.getEntity() == entity) {
          log.debug("[Access allowed] EasyNPC {} and entity {} are the same!", easyNPC, entity);
          return true;
        }

        // Allow access from NPCs with the same owner data.
        if (easyNPC != null
            && easyNPC.getEasyNPCOwnerData() != null
            && entity instanceof EasyNPC<?> easyNPCEntity
            && easyNPCEntity.getEasyNPCOwnerData() != null) {
          OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
          OwnerData<?> ownerDataEntity = easyNPCEntity.getEasyNPCOwnerData();
          boolean ownerDataAccess =
              (!ownerData.hasOwner() && !ownerDataEntity.hasOwner())
                  || (ownerData.getOwner() != null
                      && ownerData.getOwner().equals(ownerDataEntity.getOwner()));
          if (ownerDataAccess) {
            log.debug(
                "[Access allowed] EasyNPC {} and entity {} has same owner data!",
                easyNPC,
                easyNPCEntity);
          } else {
            log.error(
                "[Access denied] EasyNPC {} and entity {} has different owner data!",
                easyNPC,
                easyNPCEntity);
          }
          return ownerDataAccess;
        }

        log.error(
            "[Access denied] Entity {} tried to access EasyNPC {} with UUID {}!",
            entity,
            easyNPC,
            uuid);
        return false;
      } catch (CommandSyntaxException entityException) {
        log.warn(
            "[Access skipped] EasyNPC with UUID {} was not executed by a player or entity!", uuid);
        return true;
      }
    }
  }

  public static boolean hasAccess(ServerPlayer serverPlayer, UUID uuid) {
    return getEasyNPCEntityByUUID(uuid, serverPlayer) != null;
  }

  public static EasyNPC<?> getEasyNPCEntityByUUID(UUID uuid, ServerPlayer serverPlayer) {
    if (serverPlayer == null || uuid == null) {
      return null;
    }

    // Get EasyNPC entity by UUID.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("[{}:{}] Unable to get valid entity!", uuid, serverPlayer);
      return null;
    }

    // Check if player has permission to access the entity.
    if (!serverPlayer.isCreative()
        && !easyNPC.getEasyNPCOwnerData().isOwner(serverPlayer)
        && serverPlayer.getServer() != null
        && serverPlayer.getServer().getProfilePermissions(serverPlayer.getGameProfile())
            < Commands.LEVEL_GAMEMASTERS) {
      log.error("[{}:{}] Player has no permission to access {}!", uuid, serverPlayer, easyNPC);
      return null;
    }

    return easyNPC;
  }
}
