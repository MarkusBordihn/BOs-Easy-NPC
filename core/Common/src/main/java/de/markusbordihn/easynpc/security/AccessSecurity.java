/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.security;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class AccessSecurity {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private AccessSecurity() {}

  public static SecurityDecision checkAccess(CommandSourceStack context, UUID uuid) {
    if (context == null || uuid == null) {
      return new SecurityDecision(false, SecurityDecisionReason.INVALID_CONTEXT);
    }

    if (context.hasPermission(CommandPermissionLevel.GAMEMASTERS.minecraftLevel())) {
      return new SecurityDecision(true, SecurityDecisionReason.ADMIN_OVERRIDE);
    }

    try {
      ServerPlayer serverPlayer = context.getPlayerOrException();
      return checkAccess(serverPlayer, uuid);
    } catch (CommandSyntaxException serverPlayerException) {
      return checkEntityAccess(context, uuid);
    }
  }

  public static SecurityDecision checkAccess(ServerPlayer serverPlayer, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return new SecurityDecision(false, SecurityDecisionReason.INVALID_CONTEXT);
    }

    return checkAccess(serverPlayer, easyNPC.getEntityUUID());
  }

  public static SecurityDecision checkAccess(ServerPlayer serverPlayer, UUID uuid) {
    EasyNPC<?> easyNPC = getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      return new SecurityDecision(false, SecurityDecisionReason.INSUFFICIENT_PERMISSION);
    }

    ActorSecurityContext actorSecurityContext = CommandSecurity.getActorContext(serverPlayer);
    if (actorSecurityContext != null && actorSecurityContext.admin()) {
      return new SecurityDecision(true, SecurityDecisionReason.ADMIN_OVERRIDE);
    }

    if (serverPlayer != null && serverPlayer.isCreative()) {
      return new SecurityDecision(true, SecurityDecisionReason.CREATIVE_OVERRIDE);
    }

    return new SecurityDecision(true, SecurityDecisionReason.OWNER);
  }

  public static EasyNPC<?> getEasyNPCEntityByUUID(UUID uuid, ServerPlayer serverPlayer) {
    if (serverPlayer == null || uuid == null) {
      return null;
    }

    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("[{}:{}] Unable to get valid entity!", uuid, serverPlayer);
      return null;
    }

    ActorSecurityContext actorSecurityContext = CommandSecurity.getActorContext(serverPlayer);
    boolean hasAdminAccess = actorSecurityContext != null && actorSecurityContext.admin();
    if (!serverPlayer.isCreative()
        && !hasAdminAccess
        && !easyNPC.getEasyNPCOwnerData().isNPCOwner(serverPlayer)) {
      log.error("[{}:{}] Player has no permission to access {}!", uuid, serverPlayer, easyNPC);
      return null;
    }

    return easyNPC;
  }

  private static SecurityDecision checkEntityAccess(CommandSourceStack context, UUID uuid) {
    try {
      Entity entity = context.getEntityOrException();
      EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, context.getLevel());
      if (easyNPC != null && easyNPC.getEntity() == entity) {
        return new SecurityDecision(true, SecurityDecisionReason.SAME_ENTITY);
      }

      if (hasSameOwner(easyNPC, entity)) {
        return new SecurityDecision(true, SecurityDecisionReason.SAME_OWNER);
      }

      log.error(
          "[Access denied] Entity {} tried to access EasyNPC {} with UUID {}!",
          entity,
          easyNPC,
          uuid);
      return new SecurityDecision(false, SecurityDecisionReason.INSUFFICIENT_PERMISSION);
    } catch (CommandSyntaxException entityException) {
      return new SecurityDecision(true, SecurityDecisionReason.SERVER_SOURCE);
    }
  }

  private static boolean hasSameOwner(EasyNPC<?> easyNPC, Entity entity) {
    if (easyNPC == null
        || easyNPC.getEasyNPCOwnerData() == null
        || !(entity instanceof EasyNPC<?> easyNPCEntity)
        || easyNPCEntity.getEasyNPCOwnerData() == null) {
      return false;
    }

    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    OwnerDataCapable<?> ownerDataEntity = easyNPCEntity.getEasyNPCOwnerData();
    return ownerData.getOwnerUUID() != null
        && ownerData.getOwnerUUID().equals(ownerDataEntity.getOwnerUUID());
  }
}
