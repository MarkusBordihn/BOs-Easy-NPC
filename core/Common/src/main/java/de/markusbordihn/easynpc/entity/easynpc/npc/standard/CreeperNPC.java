package de.markusbordihn.easynpc.entity.easynpc.npc.standard;

import de.markusbordihn.easynpc.api.npc.base.CreeperBase;
import de.markusbordihn.easynpc.data.npc.DefaultNPCType;
import de.markusbordihn.easynpc.data.npc.NPCType;
import de.markusbordihn.easynpc.entity.easynpc.npc.StandardEasyNPC;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.level.Level;

public class CreeperNPC extends CreeperBase implements StandardEasyNPC<CreeperBase> {

  public static final DefaultNPCType NPC_TYPE = DefaultNPCType.CREEPER;

  public CreeperNPC(EntityType<? extends Creeper> entityType, Level level) {
    super(entityType, level);
  }

  @Override
  public NPCType getNPCType() {
    return NPC_TYPE;
  }
}
