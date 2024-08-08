//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by FernFlower decompiler)
//

package de.markusbordihn.easynpc.server.player;

import com.mojang.authlib.GameProfile;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.stats.Stat;

public class FakePlayer extends ServerPlayer {

  public FakePlayer(ServerLevel level, BlockPos blockPos) {
    super(level.getServer(), level, new GameProfile(UUID.randomUUID(), "FakePlayer"), null);
    this.setPos(blockPos.getX(), blockPos.getY(), blockPos.getZ());
  }

  public static boolean isInvalidFakePlayer(FakePlayer fakePlayer) {
    return !(fakePlayer instanceof FakePlayer) || !fakePlayer.isAlive();
  }

  public FakePlayer updatePosition(ServerLevel level, BlockPos blockPos) {
    if (this.level != level) {
      this.setLevel(level);
    } else if (this.blockPosition().distSqr(blockPos) > 1.0D) {
      this.setPos(blockPos.getX(), blockPos.getY(), blockPos.getZ());
    }
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
