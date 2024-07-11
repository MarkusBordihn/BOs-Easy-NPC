//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by FernFlower decompiler)
//

package de.markusbordihn.easynpc.server.player;

import com.mojang.authlib.GameProfile;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ClientInformation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.stats.Stat;

public class FakePlayer extends ServerPlayer {

  public FakePlayer(ServerLevel level, BlockPos blockPos) {
    super(
        level.getServer(),
        level,
        new GameProfile(UUID.randomUUID(), "FakePlayer"),
        ClientInformation.createDefault());
    this.setBlockPos(blockPos);
  }

  public FakePlayer setBlockPos(BlockPos blockPos) {
    this.setPos(blockPos.getX(), blockPos.getY(), blockPos.getZ());
    return this;
  }

  @Override
  public void displayClientMessage(Component chatComponent, boolean actionBar) {}

  @Override
  public void awardStat(Stat stat, int increment) {}

  @Override
  public void tick() {}

  @Override
  public void doTick() {}
}
